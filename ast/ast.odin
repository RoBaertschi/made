package ast

Command_Statement :: struct {
	identifier: string,
	commands:   [dynamic]string,
}

command_statement_destroy :: proc(command_statement: Command_Statement) {
	delete(command_statement.identifier)
	for s in command_statement.commands {
		delete(s)
	}
	delete(command_statement.commands)
}

Build_Statement :: struct {
	input_file:  string,
	output_file: string,
	rule_name:   string,
}

build_statement_destroy :: proc(build_statement: Build_Statement) {
	delete(build_statement.input_file)
	delete(build_statement.output_file)
	delete(build_statement.rule_name)
}

Statement :: union #no_nil {
	Build_Statement,
	Command_Statement,
}

statement_destroy :: proc(statement: Statement) {
    switch stmt in statement {
    case Build_Statement:
        build_statement_destroy(stmt)
    case Command_Statement:
        command_statement_destroy(stmt)
    }
}

Build_File :: struct {
	file:       string,
	statements: [dynamic]Statement,
}
build_file_destroy :: proc(build_file: Build_File) {
    delete(build_file.file)
    for s in build_file.statements {
        statement_destroy(s)
    }
    delete(build_file.statements)
}
