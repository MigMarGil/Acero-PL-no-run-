const std = @import("std");
const regions = @import("regions.zig");

pub const ActorId = u64;

pub const Actor = struct {
    id: ActorId,
    mailbox: Mailbox,
    state: ?*anyopaque,
    handler: HandlerFn,
    allocator: std.mem.Allocator,
    running: std.atomic.Atomic(bool) = std.atomic.Atomic(bool).init(true),
    
    pub const HandlerFn = *const fn (actor: *Actor, message: *anyopaque) anyerror!void;
    
    pub fn init(
        allocator: std.mem.Allocator,
        handler: HandlerFn,
        initial_state: ?*anyopaque,
    ) !*Actor {
        const actor = try allocator.create(Actor);
        errdefer allocator.destroy(actor);
        
        actor.id = generateId();
        actor.mailbox = Mailbox.init(allocator);
        actor.state = initial_state;
        actor.handler = handler;
        actor.allocator = allocator;
        
        return actor;
    }
    
    pub fn deinit(self: *Actor) void {
        self.mailbox.deinit();
        self.allocator.destroy(self);
    }
    
    pub fn send(self: *Actor, message: *anyopaque) !void {
        try self.mailbox.put(message);
    }
    
    pub fn run(self: *Actor) !void {
        while (self.running.load(.SeqCst)) {
            if (self.mailbox.get()) |message| {
                try self.handler(self, message);
                // Message is freed by the mailbox
            } else {
                // Mailbox empty, yield
                std.time.sleep(1000); // 1 microsecond
            }
        }
    }
    
    pub fn stop(self: *Actor) void {
        self.running.store(false, .SeqCst);
    }
    
    fn generateId() ActorId {
        var rng = std.rand.DefaultPrng.init(@intCast(u64, std.time.microTimestamp()));
        return rng.random().int(ActorId);
    }
};

pub const Mailbox = struct {
    queue: std.atomic.Queue(*anyopaque),
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) Mailbox {
        return .{
            .queue = std.atomic.Queue(*anyopaque).init(),
            .allocator = allocator,
        };
    }
    
    pub fn deinit(self: *Mailbox) void {
        while (self.queue.get()) |node| {
            self.allocator.destroy(node);
        }
    }
    
    pub fn put(self: *Mailbox, message: *anyopaque) !void {
        const node = try self.allocator.create(std.atomic.Queue(*anyopaque).Node);
        node.data = message;
        self.queue.put(node);
    }
    
    pub fn get(self: *Mailbox) ?*anyopaque {
        if (self.queue.get()) |node| {
            const message = node.data;
            self.allocator.destroy(node);
            return message;
        }
        return null;
    }
    
    pub fn isEmpty(self: *const Mailbox) bool {
        return self.queue.isEmpty();
    }
};

pub const ActorSystem = struct {
    allocator: std.mem.Allocator,
    actors: std.AutoHashMap(ActorId, *Actor),
    threads: std.ArrayList(*std.Thread),
    running: std.atomic.Atomic(bool) = std.atomic.Atomic(bool).init(true),
    
    pub fn init(allocator: std.mem.Allocator) !ActorSystem {
        return .{
            .allocator = allocator,
            .actors = std.AutoHashMap(ActorId, *Actor).init(allocator),
            .threads = std.ArrayList(*std.Thread).init(allocator),
        };
    }
    
    pub fn deinit(self: *ActorSystem) void {
        self.running.store(false, .SeqCst);
        
        // Stop all actors
        var it = self.actors.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.stop();
        }
        
        // Wait for threads
        for (self.threads.items) |thread| {
            thread.join();
            self.allocator.destroy(thread);
        }
        
        // Clean up actors
        it = self.actors.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        
        self.actors.deinit();
        self.threads.deinit();
    }
    
    pub fn spawn(
        self: *ActorSystem,
        comptime StateType: type,
        comptime MessageType: type,
        comptime handler: fn (state: *StateType, message: MessageType) anyerror!void,
        initial_state: StateType,
    ) !ActorId {
        const actor_allocator = regions.Region.heap.allocator();
        
        // Create state
        const state = try actor_allocator.create(StateType);
        state.* = initial_state;
        
        // Create actor with typed handler
        const wrapped_handler: Actor.HandlerFn = struct {
            fn f(actor: *Actor, raw_message: *anyopaque) anyerror!void {
                const state_ptr = @ptrCast(*StateType, @alignCast(@alignOf(StateType), actor.state));
                const message_ptr = @ptrCast(*MessageType, @alignCast(@alignOf(MessageType), raw_message));
                try handler(state_ptr, message_ptr.*);
                
                // Free the message
                const message_allocator = regions.Region.message.allocator();
                message_allocator.destroy(message_ptr);
            }
        }.f;
        
        const actor = try Actor.init(actor_allocator, wrapped_handler, state);
        errdefer actor_allocator.destroy(actor);
        
        try self.actors.put(actor.id, actor);
        
        // Start actor thread
        const thread = try self.allocator.create(std.Thread);
        errdefer self.allocator.destroy(thread);
        
        thread.* = try std.Thread.spawn(.{}, actorRunner, .{actor});
        try self.threads.append(thread);
        
        return actor.id;
    }
    
    pub fn send(
        self: *ActorSystem,
        actor_id: ActorId,
        comptime MessageType: type,
        message: MessageType,
    ) !void {
        if (self.actors.get(actor_id)) |actor| {
            const message_allocator = regions.Region.message.allocator();
            const message_ptr = try message_allocator.create(MessageType);
            message_ptr.* = message;
            
            try actor.send(message_ptr);
        } else {
            return error.ActorNotFound;
        }
    }
    
    pub fn stop(self: *ActorSystem, actor_id: ActorId) !void {
        if (self.actors.get(actor_id)) |actor| {
            actor.stop();
            _ = self.actors.remove(actor_id);
        } else {
            return error.ActorNotFound;
        }
    }
    
    fn actorRunner(actor: *Actor) void {
        actor.run() catch |err| {
            std.debug.print("Actor {} error: {}\n", .{ actor.id, err });
        };
        actor.deinit();
    }
    
    pub fn stats(self: *const ActorSystem) ActorSystemStats {
        var total_messages: usize = 0;
        var active_actors: usize = 0;
        
        var it = self.actors.iterator();
        while (it.next()) |entry| {
            const actor = entry.value_ptr.*;
            // TODO: Collect more stats from actors
            if (actor.running.load(.SeqCst)) {
                active_actors += 1;
            }
        }
        
        return .{
            .total_actors = self.actors.count(),
            .active_actors = active_actors,
            .total_threads = self.threads.items.len,
            .total_messages = total_messages,
        };
    }
};

pub const ActorSystemStats = struct {
    total_actors: usize,
    active_actors: usize,
    total_threads: usize,
    total_messages: usize,
    
    pub fn format(self: ActorSystemStats, writer: anytype) !void {
        try writer.print(
            \\Actor System Statistics:
            \\  Total actors: {}
            \\  Active actors: {}
            \\  Threads: {}
            \\  Total messages: {}
        , .{
            self.total_actors,
            self.active_actors,
            self.total_threads,
            self.total_messages,
        });
    }
};

// Message type registry for dynamic dispatch
pub const MessageType = struct {
    name: []const u8,
    size: usize,
    align: usize,
    handler: *const fn (*anyopaque, *anyopaque) anyerror!void,
};

pub const MessageRegistry = struct {
    allocator: std.mem.Allocator,
    types: std.StringHashMap(MessageType),
    
    pub fn init(allocator: std.mem.Allocator) MessageRegistry {
        return .{
            .allocator = allocator,
            .types = std.StringHashMap(MessageType).init(allocator),
        };
    }
    
    pub fn deinit(self: *MessageRegistry) void {
        self.types.deinit();
    }
    
    pub fn register(
        self: *MessageRegistry,
        comptime MessageType: type,
        comptime handler: fn (*anyopaque, MessageType) anyerror!void,
    ) !void {
        const wrapped_handler: fn (*anyopaque, *anyopaque) anyerror!void = struct {
            fn f(state: *anyopaque, raw_message: *anyopaque) anyerror!void {
                const message_ptr = @ptrCast(*MessageType, @alignCast(@alignOf(MessageType), raw_message));
                try handler(state, message_ptr.*);
            }
        }.f;
        
        const type_info = @typeInfo(MessageType);
        const type_name = @typeName(MessageType);
        
        try self.types.put(type_name, .{
            .name = type_name,
            .size = @sizeOf(MessageType),
            .align = @alignOf(MessageType),
            .handler = wrapped_handler,
        });
    }
    
    pub fn handle(
        self: *MessageRegistry,
        type_name: []const u8,
        state: *anyopaque,
        message: *anyopaque,
    ) !void {
        if (self.types.get(type_name)) |msg_type| {
            try msg_type.handler(state, message);
        } else {
            return error.MessageTypeNotRegistered;
        }
    }
};

// Channel-based communication (alternative to actors)
pub const Channel = struct {
    buffer: []u8,
    head: usize = 0,
    tail: usize = 0,
    count: usize = 0,
    capacity: usize,
    item_size: usize,
    mutex: std.Thread.Mutex = .{},
    not_empty: std.Thread.Condition = .{},
    not_full: std.Thread.Condition = .{},
    closed: bool = false,
    
    pub fn init(allocator: std.mem.Allocator, comptime T: type, capacity: usize) !*Channel {
        const self = try allocator.create(Channel);
        errdefer allocator.destroy(self);
        
        self.buffer = try allocator.alloc(u8, capacity * @sizeOf(T));
        self.capacity = capacity;
        self.item_size = @sizeOf(T);
        
        return self;
    }
    
    pub fn deinit(self: *Channel, allocator: std.mem.Allocator) void {
        allocator.free(self.buffer);
        allocator.destroy(self);
    }
    
    pub fn send(self: *Channel, comptime T: type, item: T) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        
        while (self.count == self.capacity and !self.closed) {
            self.not_full.wait(&self.mutex);
        }
        
        if (self.closed) {
            return error.ChannelClosed;
        }
        
        const ptr = @ptrCast(*T, @alignCast(@alignOf(T), &self.buffer[self.tail * self.item_size]));
        ptr.* = item;
        
        self.tail = (self.tail + 1) % self.capacity;
        self.count += 1;
        
        self.not_empty.signal();
    }
    
    pub fn receive(self: *Channel, comptime T: type) !T {
        self.mutex.lock();
        defer self.mutex.unlock();
        
        while (self.count == 0 and !self.closed) {
            self.not_empty.wait(&self.mutex);
        }
        
        if (self.closed and self.count == 0) {
            return error.ChannelClosed;
        }
        
        const ptr = @ptrCast(*T, @alignCast(@alignOf(T), &self.buffer[self.head * self.item_size]));
        const item = ptr.*;
        
        self.head = (self.head + 1) % self.capacity;
        self.count -= 1;
        
        self.not_full.signal();
        
        return item;
    }
    
    pub fn close(self: *Channel) void {
        self.mutex.lock();
        self.closed = true;
        self.mutex.unlock();
        
        self.not_empty.broadcast();
        self.not_full.broadcast();
    }
    
    pub fn isClosed(self: *Channel) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.closed;
    }
    
    pub fn len(self: *Channel) usize {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.count;
    }
};
