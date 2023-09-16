const std = @import("std");

const String = []const u8;

pub const Value = struct {
    name: String,
    values: std.ArrayList(Value),
    pub fn init(alloc: std.mem.Allocator, name: []const u8) Value {
        return .{ .name = name, .values = std.ArrayList(Value).init(alloc) };
    }
    pub fn deinit(self: Value) void {
        for (self.values.items) |item| item.deinit();
        self.values.deinit();
    }
    pub fn format(self: Value, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "Name:{s}\n", .{self.name});
        for (self.values.items) |item| {
            try std.fmt.format(writer, "{}", .{item});
        }
    }
};

const TestCase = struct {
    raw: []const u8,
    tokens: []const TokenIterator.Token,
};

test {
    const testCases = [_]TestCase{.{
        .raw =
        \\(pin input line (at -12.7 -2.54 0) (length 5.08)
        \\(name "D7" (effects (font (size 1.27 1.27))))
        \\(number "10" (effects (font (size 1.27 1.27))))
        \\)
        ,
        .tokens = &.{ .open, .{ .token = "pin" }, .{ .token = "input" }, .{ .token = "line" }, .open, .{ .token = "at" }, .{ .token = "-12.7" }, .{ .token = "-2.54" }, .{ .token = "0" }, .close, .open, .{ .token = "length" }, .{ .token = "5.08" }, .close, .open, .{ .token = "name" }, .{ .token = "D7" }, .open, .{ .token = "effects" }, .open, .{ .token = "font" }, .open, .{ .token = "size" }, .{ .token = "1.27" }, .{ .token = "1.27" }, .close, .close, .close, .close, .open, .{ .token = "number" }, .{ .token = "10" }, .open, .{ .token = "effects" }, .open, .{ .token = "font" }, .open, .{ .token = "size" }, .{ .token = "1.27" }, .{ .token = "1.27" }, .close, .close, .close, .close, .close },
    }};
    const alloc = std.testing.allocator;

    for (testCases) |tc| {
        var iter = TokenIterator{ .text = tc.raw };
        var count: usize = 0;
        var value = try parseSexp(tc.raw, alloc);
        defer value.deinit();
        while (try iter.next()) |t| {
            defer count += 1;
            errdefer std.log.err("idx[{}] expected:[{}] got:[{}]", .{ count, tc.tokens[count], t });
            try std.testing.expect(t.cmp(tc.tokens[count]));
        }
    }
}

pub const ValueMap = struct {
    items: std.ArrayList(Node),
    alloc: std.mem.Allocator,
    pub const NodeIdx = usize;
    pub const Node = struct {
        name: []const u8,
        parent: ?NodeIdx,
        child: std.ArrayList(NodeIdx),
        pub fn init(alloc: std.mem.Allocator, name: []const u8, parent: ?NodeIdx) Node {
            return .{
                .name = name,
                .parent = parent,
                .child = std.ArrayList(NodeIdx).init(alloc),
            };
        }
        pub fn deinit(self: Node) void {
            self.child.deinit();
        }
    };
    pub fn init(alloc: std.mem.Allocator) ValueMap {
        return .{
            .items = std.ArrayList(Node).init(alloc),
            .alloc = alloc,
        };
    }
    pub fn deinit(self: ValueMap) void {
        for (self.items.items) |i| {
            i.deinit();
        }
        self.items.deinit();
    }
    pub fn addNode(self: *ValueMap, name: []const u8, parent: ?NodeIdx) !NodeIdx {
        var n = Node.init(self.alloc, name, parent);
        errdefer n.deinit();
        const idx = self.items.items.len;
        try self.items.append(n);
        return idx;
    }
    pub fn format(self: ValueMap, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        var idx: ?usize = 0;

        while (idx) |i| {}
    }
};

pub fn parseSexp(text: []const u8, alloc: std.mem.Allocator) !ValueMap {
    var iter = TokenIterator{ .text = text };
    var value = ValueMap.init(alloc);
    errdefer value.deinit();
    var cur = try value.addNode("", null);

    while (try iter.next()) |t| {
        switch (t) {
            .open => {
                switch (((try iter.peek()) orelse return error.UnexpetctedEnd)) {
                    .open, .close => return error.ExpectedName,
                    .token => |name| {
                        _ = try iter.next();
                        const idx = try value.addNode(name, cur);
                        try value.items.items[cur].child.append(idx);
                        cur = idx;
                    },
                }
            },
            .close => {
                if (value.items.items[cur].parent) |p| {
                    cur = p;
                } else {
                    return error.UnexpectedParen;
                }
            },
            .token => |name| {
                const idx = try value.addNode(name, cur);
                try value.items.items[cur].child.append(idx);
            },
        }
    }
    return value;
}

const TokenIterator = struct {
    pub const Token = union(enum) {
        open: void,
        close: void,
        token: []const u8,
        pub fn cmp(self: Token, other: Token) bool {
            switch (self) {
                .open => {
                    switch (other) {
                        .open => return true,
                        else => return false,
                    }
                },
                .close => {
                    switch (other) {
                        .close => return true,
                        else => return false,
                    }
                },
                .token => |t| switch (other) {
                    .token => |tt| return std.mem.eql(u8, t, tt),
                    else => return false,
                },
            }
        }
        pub fn format(self: Token, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .open => try std.fmt.format(writer, ".open,", .{}),
                .close => try std.fmt.format(writer, ".close,", .{}),
                .token => |t| try std.fmt.format(writer, ".{{.token = \"{s}\"}},", .{t}),
            }
        }
    };
    text: []const u8,
    idx: usize = 0,
    pub fn peek(self: TokenIterator) !?Token {
        var tmp = self;
        return tmp.next();
    }
    pub fn next(self: *TokenIterator) !?Token {
        if (self.idx >= self.text.len) return null;
        self.skipWhitespace();
        const tokenStr = try self.readToken();
        if (tokenStr) |t| {
            if (std.mem.eql(u8, "(", t)) return Token.open;
            if (std.mem.eql(u8, ")", t)) return Token.close;
            return Token{ .token = t };
        } else {
            return null;
        }
    }

    fn peekChar(self: TokenIterator) ?u8 {
        if (self.idx >= self.text.len) return null;
        return self.text[self.idx];
    }

    fn nextChar(self: *TokenIterator) ?u8 {
        if (self.idx >= self.text.len) return null;
        defer self.idx += 1;
        return self.text[self.idx];
    }

    fn readToken(self: *TokenIterator) !?[]const u8 {
        const start = self.idx;
        var isQuoted: bool = false;
        while (self.peekChar()) |ch| {
            switch (ch) {
                '(' => {
                    _ = self.nextChar();
                    if (!isQuoted) {
                        return self.text[start..self.idx];
                    }
                },
                ')' => {
                    if (start == self.idx) {
                        _ = self.nextChar();
                        return self.text[start..][0..1];
                    } else {
                        if (!isQuoted) {
                            return self.text[start..self.idx];
                        }
                    }
                },
                '"' => {
                    _ = self.nextChar();
                    if (isQuoted) {
                        return self.text[start + 1 .. self.idx - 1]; // get rid of the '"'
                    } else if (self.idx - start == 1) {
                        isQuoted = true;
                    } else {
                        return error.InvalidToken;
                    }
                },
                else => {
                    _ = self.nextChar();
                    if (std.ascii.isWhitespace(ch)) {
                        if (!isQuoted) {
                            return self.text[start .. self.idx - 1];
                        }
                    }
                },
            }
        }
        return self.text[start..];
    }
    pub fn skipWhitespace(self: *TokenIterator) void {
        while (std.ascii.isWhitespace(self.peekChar() orelse return)) {
            _ = self.nextChar();
        }
    }
};
