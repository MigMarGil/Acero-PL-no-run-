const std = @import("std");

pub const Region = enum {
    stack,
    arena,
    heap,
    global,
    thread,
    message,
    
    pub fn allocator(self: Region) std.mem.Allocator {
        return switch (self) {
            .stack => stack_allocator,
            .arena => arena_allocator,
            .heap => heap_allocator,
            .global => global_allocator,
            .thread => thread_allocator,
            .message => message_allocator,
        };
    }
    
    pub fn defaultAllocator(self: Region) *RegionAllocator {
        return switch (self) {
            .stack => &stack_default,
            .arena => &arena_default,
            .heap => &heap_default,
            .global => &global_default,
            .thread => &thread_default,
            .message => &message_default,
        };
    }
};

pub const RegionAllocator = struct {
    region: Region,
    parent: ?*RegionAllocator = null,
    backing_allocator: std.mem.Allocator,
    
    // Statistics
    bytes_allocated: usize = 0,
    bytes_freed: usize = 0,
    allocation_count: usize = 0,
    free_count: usize = 0,
    
    pub fn init(region: Region, backing_allocator: std.mem.Allocator) RegionAllocator {
        return .{
            .region = region,
            .backing_allocator = backing_allocator,
        };
    }
    
    pub fn allocator(self: *RegionAllocator) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }
    
    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        _ = ret_addr;
        const self = @ptrCast(*RegionAllocator, @alignCast(@alignOf(RegionAllocator), ctx));
        
        const ptr = self.backing_allocator.rawAlloc(len, ptr_align, ret_addr) orelse return null;
        
        self.bytes_allocated += len;
        self.allocation_count += 1;
        
        return ptr;
    }
    
    fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        _ = ret_addr;
        const self = @ptrCast(*RegionAllocator, @alignCast(@alignOf(RegionAllocator), ctx));
        
        if (self.backing_allocator.rawResize(buf, buf_align, new_len, ret_addr)) {
            if (new_len > buf.len) {
                self.bytes_allocated += new_len - buf.len;
            } else {
                self.bytes_freed += buf.len - new_len;
            }
            return true;
        }
        
        return false;
    }
    
    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        _ = ret_addr;
        const self = @ptrCast(*RegionAllocator, @alignCast(@alignOf(RegionAllocator), ctx));
        
        self.backing_allocator.rawFree(buf, buf_align, ret_addr);
        
        self.bytes_freed += buf.len;
        self.free_count += 1;
    }
    
    pub fn stats(self: *const RegionAllocator) AllocatorStats {
        return .{
            .bytes_allocated = self.bytes_allocated,
            .bytes_freed = self.bytes_freed,
            .allocation_count = self.allocation_count,
            .free_count = self.free_count,
            .live_bytes = self.bytes_allocated - self.bytes_freed,
            .live_allocations = self.allocation_count - self.free_count,
        };
    }
    
    pub fn reset(self: *RegionAllocator) void {
        // For arena allocators, reset statistics
        if (self.region == .arena) {
            self.bytes_allocated = 0;
            self.bytes_freed = 0;
            self.allocation_count = 0;
            self.free_count = 0;
        }
    }
};

pub const AllocatorStats = struct {
    bytes_allocated: usize,
    bytes_freed: usize,
    allocation_count: usize,
    free_count: usize,
    live_bytes: usize,
    live_allocations: usize,
    
    pub fn format(self: AllocatorStats, writer: anytype) !void {
        try writer.print(
            \\Allocator Statistics:
            \\  Bytes allocated: {}
            \\  Bytes freed:     {}
            \\  Live bytes:      {}
            \\  Allocations:     {}
            \\  Frees:           {}
            \\  Live allocations: {}
        , .{
            self.bytes_allocated,
            self.bytes_freed,
            self.live_bytes,
            self.allocation_count,
            self.free_count,
            self.live_allocations,
        });
    }
};

// Specialized allocators for each region type
pub const ArenaAllocator = struct {
    region: RegionAllocator,
    chunks: std.ArrayList(Chunk),
    current_chunk: ?*Chunk,
    
    const Chunk = struct {
        data: []u8,
        used: usize,
        
        pub fn init(allocator: std.mem.Allocator, size: usize) !Chunk {
            return .{
                .data = try allocator.alloc(u8, size),
                .used = 0,
            };
        }
        
        pub fn deinit(self: *Chunk, allocator: std.mem.Allocator) void {
            allocator.free(self.data);
        }
    };
    
    pub fn init(backing_allocator: std.mem.Allocator) !ArenaAllocator {
        return .{
            .region = RegionAllocator.init(.arena, backing_allocator),
            .chunks = std.ArrayList(Chunk).init(backing_allocator),
            .current_chunk = null,
        };
    }
    
    pub fn deinit(self: *ArenaAllocator) void {
        for (self.chunks.items) |*chunk| {
            chunk.deinit(self.region.backing_allocator);
        }
        self.chunks.deinit();
    }
    
    pub fn allocator(self: *ArenaAllocator) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = arenaAlloc,
                .resize = arenaResize,
                .free = arenaFree,
            },
        };
    }
    
    fn arenaAlloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        _ = ret_addr;
        const self = @ptrCast(*ArenaAllocator, @alignCast(@alignOf(ArenaAllocator), ctx));
        
        // Try current chunk first
        if (self.current_chunk) |chunk| {
            const start = std.mem.alignForward(chunk.used, ptr_align);
            if (start + len <= chunk.data.len) {
                chunk.used = start + len;
                const ptr = @ptrCast([*]u8, &chunk.data[start]);
                
                self.region.bytes_allocated += len;
                self.region.allocation_count += 1;
                
                return ptr;
            }
        }
        
        // Allocate new chunk (at least 64KB or 2x requested size)
        const chunk_size = std.math.max(65536, len * 2);
        var chunk = Chunk.init(self.region.backing_allocator, chunk_size) catch return null;
        
        const start = std.mem.alignForward(0, ptr_align);
        chunk.used = start + len;
        
        self.chunks.append(chunk) catch {
            chunk.deinit(self.region.backing_allocator);
            return null;
        };
        
        self.current_chunk = &self.chunks.items[self.chunks.items.len - 1];
        
        const ptr = @ptrCast([*]u8, &self.current_chunk.?.data[start]);
        
        self.region.bytes_allocated += len;
        self.region.allocation_count += 1;
        
        return ptr;
    }
    
    fn arenaResize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        _ = ret_addr;
        _ = ctx;
        _ = buf_align;
        
        // Arenas don't support resizing
        return new_len <= buf.len;
    }
    
    fn arenaFree(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        _ = ret_addr;
        _ = ctx;
        _ = buf;
        _ = buf_align;
        
        // Arenas free everything on reset
    }
    
    pub fn reset(self: *ArenaAllocator) void {
        for (self.chunks.items) |*chunk| {
            chunk.used = 0;
        }
        if (self.chunks.items.len > 0) {
            self.current_chunk = &self.chunks.items[0];
        }
        
        self.region.reset();
    }
    
    pub fn stats(self: *const ArenaAllocator) AllocatorStats {
        return self.region.stats();
    }
};

pub const StackAllocator = struct {
    region: RegionAllocator,
    frame_stack: std.ArrayList(Frame),
    
    const Frame = struct {
        base: usize,
        size: usize,
        
        pub fn init(base: usize, size: usize) Frame {
            return .{ .base = base, .size = size };
        }
    };
    
    pub fn init(backing_allocator: std.mem.Allocator) !StackAllocator {
        return .{
            .region = RegionAllocator.init(.stack, backing_allocator),
            .frame_stack = std.ArrayList(Frame).init(backing_allocator),
        };
    }
    
    pub fn deinit(self: *StackAllocator) void {
        self.frame_stack.deinit();
    }
    
    pub fn pushFrame(self: *StackAllocator, size: usize) !void {
        const frame = Frame.init(self.region.bytes_allocated, size);
        try self.frame_stack.append(frame);
    }
    
    pub fn popFrame(self: *StackAllocator) void {
        if (self.frame_stack.popOrNull()) |frame| {
            // Free everything allocated in this frame
            self.region.bytes_freed += self.region.bytes_allocated - frame.base;
            self.region.bytes_allocated = frame.base;
        }
    }
    
    pub fn allocator(self: *StackAllocator) std.mem.Allocator {
        return self.region.allocator();
    }
};

// Global instances
var stack_default: RegionAllocator = undefined;
var arena_default: RegionAllocator = undefined;
var heap_default: RegionAllocator = undefined;
var global_default: RegionAllocator = undefined;
var thread_default: RegionAllocator = undefined;
var message_default: RegionAllocator = undefined;

pub fn initGlobalAllocators() void {
    const page_allocator = std.heap.page_allocator;
    
    stack_default = RegionAllocator.init(.stack, page_allocator);
    arena_default = RegionAllocator.init(.arena, page_allocator);
    heap_default = RegionAllocator.init(.heap, page_allocator);
    global_default = RegionAllocator.init(.global, page_allocator);
    thread_default = RegionAllocator.init(.thread, page_allocator);
    message_default = RegionAllocator.init(.message, page_allocator);
}

// Region-aware allocation functions
pub fn allocate(region: Region, comptime T: type, count: usize) ![]T {
    const allocator = region.allocator();
    return try allocator.alloc(T, count);
}

pub fn allocateOne(region: Region, comptime T: type) !*T {
    const allocator = region.allocator();
    return try allocator.create(T);
}

pub fn free(region: Region, memory: anytype) void {
    const allocator = region.allocator();
    
    switch (@typeInfo(@TypeOf(memory))) {
        .Pointer => |ptr_info| {
            if (ptr_info.size == .Slice) {
                allocator.free(memory);
            } else {
                allocator.destroy(memory);
            }
        },
        else => @compileError("Expected pointer or slice"),
    }
}

pub fn reallocate(region: Region, old_memory: anytype, new_count: usize) !@TypeOf(old_memory) {
    const allocator = region.allocator();
    return try allocator.realloc(old_memory, new_count);
}

// Region checking utilities
pub fn checkRegionCompatibility(source: Region, target: Region) bool {
    // A value can be moved from source region to target region if:
    // 1. target region lives at least as long as source region
    // 2. Or we're copying the value (not moving ownership)
    
    const region_order = [_]Region{
        .stack,    // Shortest lived
        .thread,
        .message,
        .arena,
        .heap,
        .global,   // Longest lived
    };
    
    const source_index = for (region_order, 0..) |r, i| {
        if (r == source) break i;
    } else region_order.len;
    
    const target_index = for (region_order, 0..) |r, i| {
        if (r == target) break i;
    } else region_order.len;
    
    return target_index >= source_index;
}

pub fn getDefaultAllocator() std.mem.Allocator {
    return Region.stack.allocator();
}
