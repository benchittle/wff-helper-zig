const std = @import("std");
const debug = std.debug;

const stdout = std.io.getStdOut().writer();

const lex = @import("wff-lexing.zig");

const Variable = struct {
    const Self = @This();

    id: usize,

    fn eql(self: Self, other: Self) bool {
        return self.id == other.id;
    }
};

const Terminal = struct {
    const Self = @This();

    id: usize,

    fn eql(self: Self, other: Self) bool {
        return self.id == other.id;
    }
};

const Symbol = union(enum) {
    const Self = @This();

    variable: Variable,
    terminal: Terminal,

    fn eql(self: Self, other: Self) bool {
        return switch(self) {
            .variable => |v1| switch(other) {
                .variable => |v2| v1.eql(v2),
                .terminal => |_| false,
            },
            .terminal => |t1| switch(other) {
                .variable => |_| false,
                .terminal => |t2| t1.eql(t2),
            }
        };
    }
};

const Production = struct {
    // TODO: Both of these helper structs should probably have dynamically 
    // allocated strings
    
    lhs: Variable,
    rhs: []const Symbol,
};

const ProductionInstance = struct {
    const Self = @This();

    production: Production,
    cursor: usize,

    fn fromProduction(production: Production) Self {
        return ProductionInstance{ .production = production, .cursor = 0 };
    }

    fn readCursor(self: Self) ?Symbol {
        if (self.cursor >= self.production.rhs.len) {
            return null;
        } else {
            return self.production.rhs[self.cursor];
        }
    }

    fn copyAdvanceCursor(self: Self) Self {
        return ProductionInstance{.production = self.production, .cursor = self.cursor + 1};
    }
};

const Grammar = struct {
    const Self = @This();

    rules: []const Production,
    variables: []const * u8,
    terminals: []const lex.Token,

    fn getSymbolCount(self: Self) usize {
        return self.variables.len + self.terminals.len;
    }

    pub fn printProductionInstance(_: Self, prod: ProductionInstance) !void {
        try stdout.print("{d}", .{prod.production.lhs.id});
        try stdout.print(" ->", .{});
        for (prod.production.rhs[0..prod.cursor]) |sym| {
            switch(sym) {
                .terminal => |t| try stdout.print(" {d}", .{t.id}),
                .variable => |v| try stdout.print(" {d}", .{v.id}),
            }
        }
        try stdout.print(" *", .{});
        for (prod.production.rhs[prod.cursor..]) |sym| {
            switch(sym) {
                .terminal => |t| try stdout.print(" {d}", .{t.id}),
                .variable => |v| try stdout.print(" {d}", .{v.id}),
            }
        }
        try stdout.print("\n", .{});
    }
};

const ParseTable = struct {
    const Self = @This();

    grammar: Grammar,


    fn expandProductions(self: Self, allocator: std.mem.Allocator, productions: []const ProductionInstance) ![]std.ArrayList(ProductionInstance) {
        // const? 
        var branches = try allocator.alloc(std.ArrayList(ProductionInstance), self.grammar.getSymbolCount());
        errdefer allocator.free(branches);
        
        for (branches, 0..) |_, i| {
            branches[i] = std.ArrayList(ProductionInstance).init(allocator);
        }
        errdefer {
            for (branches) |list| {
                list.deinit();
            }
            allocator.free(branches);
        }

        var stack = std.ArrayList(ProductionInstance).init(allocator);
        defer stack.deinit();

        try stack.appendSlice(productions);

        // Expand all given ProductionInstances and track all of the symbols
        // currently being read.
        while (stack.popOrNull()) |prod| {
            switch (prod.readCursor() orelse continue) {
                .variable => |v| {
                    if (branches[v.id].items.len == 0) {
                        for (self.grammar.rules) |rule| {
                            if (v.eql(rule.lhs)) {
                                try stack.append(ProductionInstance.fromProduction(rule));
                            }
                        }
                    }
                    try branches[v.id].append(prod.copyAdvanceCursor());
                },
                .terminal => |t| try branches[t.id].append(prod.copyAdvanceCursor()), 
            }
        }

        return branches;
    }
};

test "expandProductions-grammar1_0" {
    // wff = 0
    // Proposition = 1
    // Not = 2
    // LParen = 3
    // RParen = 4
    // And = 5
    // Or = 6
    // Cond = 7
    // Bicond = 8
    // S' = 9



    var r1 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{Symbol{.terminal = Terminal{.id = 1}}}
    };

    var r2 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 2}}, 
            Symbol{.variable = Variable{.id = 0}}
        }
    };

    var r3 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 3}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 5}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };

    var r4 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 3}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 6}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };

    var r5 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 3}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 7}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };

    var r6 = Production{
        .lhs = Variable{.id = 0},
        .rhs = &[_]Symbol{
            Symbol{.terminal = Terminal{.id = 3}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 8}}, 
            Symbol{.variable = Variable{.id = 0}},
            Symbol{.terminal = Terminal{.id = 4}}, 
        }
    };


    var r0 = Production{
        .lhs = Variable{.id = 9},
        .rhs = &[_] Symbol{Symbol{.variable = Variable{.id = 0}}}
    };

    var grammar = Grammar{
        .rules = &[_] Production{r0, r1, r2, r3, r4, r5, r6},
        .terminals = &[_] lex.Token {undefined} ** 8,
        .variables = &[_] *u8{undefined} ** 2,
    };
    
    var table = ParseTable{
        .grammar = grammar
    };

    const start_productions = [_]ProductionInstance {ProductionInstance.fromProduction(r0)};
    var branches = try table.expandProductions(std.testing.allocator, &start_productions);
    
    for (branches, 0..) |prods, i| {
        try stdout.print("\n{d}\n", .{i});
        for (prods.items) |p| {
            try grammar.printProductionInstance(p);
        }
    }
    defer {
        for (branches) |list| {
            list.deinit();
        }
        std.testing.allocator.free(branches);
    }

}