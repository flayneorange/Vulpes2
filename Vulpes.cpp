//---Program Options
#define enable_lexer_print 1
#define enable_parser_print 1
#define enable_linearizer_print 1
#define enable_c_backend_print 1

//---Includes
#include "foxlib_platform.hpp"

LinearAllocator heap_stack;

//---SourceSite
struct SourceSite {
	ConstString path;
	String source;
	u64 line;
	char* line_start;
	u64 column;
};

template<typename AllocatorType>
internal void write(String* buffer, SourceSite site, AllocatorType* allocator) {
	write(buffer, "File: ", allocator);
	write(buffer, site.path, allocator);
	write(buffer, " line ", allocator);
	write_uint(buffer, site.line, allocator);
	write(buffer, " column ", allocator);
	write_uint(buffer, site.column, allocator);
	write(buffer, "\n", allocator);
	
	//print the line
	//@todo print context
	//@todo print error as range rather than just start
	auto line_end = site.line_start;
	auto source_end = site.source.data + site.source.length;
	while (line_end < source_end && *line_end != '\n') {
		line_end++;
	}
	
	String line;
	zero(&line);
	line.data = site.line_start;
	line.length = line_end - site.line_start;
	fox_for (source_index, line.length) {
		if (line[source_index] == '\t') {
			write(buffer, "  ", allocator);
		} else {
			write_char(buffer, line[source_index], allocator);
		}
	}
	write(buffer, "\n", allocator);
	
	//print a cursor to the start of the site
	//-------------------------------^
	fox_for (source_index, site.column - 1) {
		if (line[source_index] == '\t') {
			write(buffer, "--", allocator);
		} else {
			write(buffer, "-", allocator);
		}
	}
	write(buffer, "^", allocator);
	write(buffer, "\n", allocator);
}

//---Diagnostics
template<typename AllocatorType>
internal void write_indent(String* buffer, fuint indent, AllocatorType* allocator) {
	fox_for (_, indent) {
		write(buffer, "  ", allocator);
	}
}

//Print unexpected end of file at <where> error message
//Where should end in a line ending
void print_unexpected_end_of_file(SourceSite site, ConstString where) {
	fox_assert(where[where.length - 1] == '\n');
	
	String error_message;
	zero(&error_message);
	write(&error_message, site, &heap_stack);
	write(&error_message, "Syntax error: unexpected end of file ", &heap_stack);
	write(&error_message, where, &heap_stack);
	print(error_message);
}

//---Lexer
bool is_space_character(char* character) {
	return *character <= ' ';
}

bool is_operator_character(char* character) {
	return find(ConstString("`~!@#$%^&*()-+=[]\\{}|;:,./"), *character);
}

bool is_number_character(char* character) {
	return '0' <= *character && *character <= '9';
}

bool is_identifier_character(char* character) {
	return !is_space_character(character)
		&& !is_operator_character(character);
}

//Language keywords/operators 
//Aligns with keyword_strings;
enum class Keyword {
	assign,
	multiply,
	divide,
	add,
	subtract,
	declare,
	function,
	return_keyword,
	integer_u8,
	integer_u16,
	integer_u32,
	integer_u64,
	integer_uint,
	integer_s8,
	integer_s16,
	integer_s32,
	integer_s64,
	integer_int,
	comma,
	open_parenthesis,
	close_parenthesis,
	open_square, //@rename should this be named open_array or something like that?
	close_square,
	open_curly,
	close_curly,
	keyword_count
};

ConstString keyword_strings[] = {
	"=",
	"*",
	"/",
	"+",
	"-",
	":",
	"function",
	"return",
	"u8",
	"u16",
	"u32",
	"u64",
	"uint",
	"s8",
	"s16",
	"s32",
	"s64",
	"int",
	",",
	"(",
	")",
	"[",
	"]",
	"{",
	"}",
};

u64 precedences[] = {
	100, // =
	300, // *
	300, // /
	200, // +
	200, // -
	150, // :
	0,   // function
	0,   // return
	0,   // u8
	0,   // u16
	0,   // u32
	0,   // u64
	0,   // uint
	0,   // s8
	0,   // s16
	0,   // s32
	0,   // s64
	0,   // int
	0,   // ,
	0,   // (
	0,   // )
	0,   // [
	0,   // ]
	0,   // {
	0,   // }
};

static_assert((fuint)Keyword::keyword_count == fox_array_length(keyword_strings)
			  && (fuint)Keyword::keyword_count == fox_array_length(precedences));

internal bool is_integer_type_keyword(Keyword keyword) {
	return Keyword::integer_u8 <= keyword && keyword <= Keyword::integer_int;
}

internal bool is_signed_integer_type_keyword(Keyword keyword) {
	fox_assert(is_integer_type_keyword(keyword));
	return keyword >= Keyword::integer_s8;
}

internal bool is_binary_operator(Keyword keyword) {
	return Keyword::assign <= keyword && keyword <= Keyword::declare;
}

bool is_right_associative(Keyword keyword) {
	if (keyword == Keyword::assign) {
		return true;
	}
	return false;
}

enum class TokenKind : u8 {
	invalid,
	keyword,
	integer,
	string,
	identifier,
};

struct Token {
	SourceSite site;
	union {
		Keyword keyword_value;
		ConstString identifier_value;
		ConstString string_value;
		u64 integer_value;
	};
	TokenKind kind;
};

//Token classification helpers
internal bool is_integer_type_keyword(Token token) {
	return token.kind == TokenKind::keyword && is_integer_type_keyword(token.keyword_value);
}

template<typename AllocatorType>
internal void write(String* buffer, Keyword keyword, AllocatorType* allocator) {
	write(buffer, keyword_strings[(fuint)keyword], allocator);
}

template<typename AllocatorType>
internal void write(String* buffer, Token token, AllocatorType* allocator) {
	switch (token.kind) {
		case TokenKind::keyword: {
			write(buffer, "Keyword (", allocator);
			write(buffer, token.keyword_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::integer: {
			write(buffer, "Integer (", allocator);
			write_uint(buffer, token.integer_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::string: {
			write(buffer, "String (", allocator);
			write(buffer, token.string_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::identifier: {
			write(buffer, "Identifier (", allocator);
			write(buffer, token.identifier_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

Optional<Array<Token>> lex(String source, ConstString path) {
	fox_assert(source);
	
	Array<Token> tokens;
	zero(&tokens);
	
	//Replace '\r\n' with ' \n'
	// and    '\r'   with '\n'
	fox_for (source_index, source.length) {
		if (source[source_index] == '\r') {
			if (source_index + 1 < source.length && source[source_index + 1] == '\n') {
				source[source_index] = ' ';
				source_index++;
			} else {
				source[source_index] = '\n';
			}
		}
	}
	
	auto cursor = source.data;
	auto source_end = source.data + source.length;
	SourceSite site_cursor;
	zero(&site_cursor);
	site_cursor.path = path;
	site_cursor.source = source;
	site_cursor.line = 1;
	site_cursor.line_start = source.data;
	site_cursor.column = 1;
	
	auto increment_cursor = [&]() {
		cursor++;
		site_cursor.column++;
	};
	
	auto add_to_cursor = [&](fuint amount) {
		cursor += amount;
		site_cursor.column += amount;
	};
	
	auto increment_cursor_to_next_line = [&]() {
		cursor++;
		site_cursor.line++;
		site_cursor.line_start = cursor;
		site_cursor.column = 1;
	};
	
	while (cursor < source_end) {
		auto skipped_any = true;
		while (skipped_any) {
			skipped_any = false;
			
			//Skip whitespace
			while (is_space_character(cursor)) {
				skipped_any = true;
				if (*cursor == '\n') {
					increment_cursor_to_next_line();
				} else {
					increment_cursor();
				}
				
				if (cursor == source_end) {
					return tokens;
				}
			}
			
			//Skip comments
			if (cursor + 1 < source_end && cursor[0] == '/') {
				if (cursor[1] == '/') {
					skipped_any = true;
					add_to_cursor(2);
					while (cursor < source_end && *cursor != '\n') {
						increment_cursor();
					}
					
					if (cursor != source_end) {
						//cursor == '\n'
						increment_cursor_to_next_line();
					}
				} else if (cursor[1] == '*') {
					auto start_of_block_comment = site_cursor;
					skipped_any = true;
					add_to_cursor(2);
					
					fuint nested_comment_counter = 1;
					while (nested_comment_counter) {
						while (cursor < source_end && *cursor != '/' && *cursor != '*') {
							if (*cursor == '\n') {
								increment_cursor_to_next_line();
							} else {
								increment_cursor();
							}
						}
						
						if (cursor + 1 < source_end) {
							if (cursor[0] == '*' && cursor[1] == '/') {
								nested_comment_counter--;
								add_to_cursor(2);
							} else if (cursor[0] == '/' && cursor[1] == '*') {
								nested_comment_counter++;
								add_to_cursor(2);
							} else {
								increment_cursor();
							}
						} else {
							//We hit the end of the file before closing our block comment
							String error_message;
							zero(&error_message);
							write(&error_message, start_of_block_comment, &heap_stack);
							write(&error_message, "Syntax error: didn't close block comment before end of file.\n", &heap_stack);
							print(error_message);
							return nil;
						}
					}
				}
			}
		}
		
		auto push_new_token = [&](TokenKind kind, SourceSite site) {
			auto new_token = push_zero(&tokens, &heap_stack);
			new_token->kind = kind;
			new_token->site = site;
			return new_token;
		};
		
		//Check for operators
		if (is_operator_character(cursor)) {
			auto operator_start_site = site_cursor;
			auto operator_start = cursor;
			
			auto operator_end_cursor = cursor;
			while (operator_end_cursor < source_end && is_operator_character(operator_end_cursor)) {
				operator_end_cursor++;
			}
			
			ConstString operator_string;
			zero(&operator_string);
			operator_string.data = operator_start;
			operator_string.length = operator_end_cursor - operator_start;
			
			auto keyword_found = false;
			while (operator_string) {
				auto keyword_index_optional = find(Array<ConstString>(keyword_strings), operator_string);
				if (keyword_index_optional) {
					auto new_token = push_new_token(TokenKind::keyword, operator_start_site);
					new_token->keyword_value = (Keyword)keyword_index_optional.value;
					add_to_cursor(operator_end_cursor - cursor);
					keyword_found = true;
					break;
				}
				
				operator_string.length--;
				operator_end_cursor--;
			}
			
			if (!keyword_found) {
				String error_message;
				zero(&error_message);
				write(&error_message, operator_start_site, &heap_stack);
				write(&error_message, "Syntax error: ", &heap_stack);
				write(&error_message, operator_string, &heap_stack);
				write(&error_message, " is not a valid operator.\n", &heap_stack);
				print(error_message);
				return nil;
			}
		}
		
		//Check for numbers
		else if (is_number_character(cursor)) {
			auto number_start_site = site_cursor;
			auto number_start = cursor;
			while (cursor < source_end && is_number_character(cursor)) {
				increment_cursor();
			}
			
			if (cursor == source_end || is_space_character(cursor) || is_operator_character(cursor)) {
				ConstString number_string;
				zero(&number_string);
				number_string.data = number_start;
				number_string.length = cursor - number_start;
				
				u64 integer_value = unsigned_integer_from_string(number_string);
				
				auto new_token = push_new_token(TokenKind::integer, number_start_site);
				new_token->integer_value = integer_value;
			} else {
				String error_message;
				zero(&error_message);
				write(&error_message, site_cursor, &heap_stack);
				write(&error_message, "Syntax error: Expected end-of-file, whitespace, or operator after integer literal.\n", &heap_stack);
				print(error_message);
				return nil;
			}
		}
		
		//Check for string literals
		else if (*cursor == '\'') {
			auto string_start_site = site_cursor;
			
			increment_cursor();
			
			String string_value;
			zero(&string_value);
			
			while (true) {
				if (*cursor == '\\') {
					if (cursor + 1 == source_end) {
						print_unexpected_end_of_file(site_cursor, "after escape sequence start.\n");
						return nil;
					}
					
					if (is_space_character(cursor + 1)) {
						String error_message;
						zero(&error_message);
						write(&error_message, site_cursor, &heap_stack);
						write(&error_message, "Syntax error: whitespace after escape sequence start.\n", &heap_stack);
						print(error_message);
						return nil;
					}
					
					switch (cursor[1]) {
						case '\\': {
							write(&string_value, "\\", &heap_stack);
						} break;
						
						case 'n': {
							write(&string_value, "\n", &heap_stack);
						} break;
						
						case '\'': {
							write(&string_value, "'", &heap_stack);
						} break;
						
						default: {
							String error_message;
							zero(&error_message);
							write(&error_message, site_cursor, &heap_stack);
							write(&error_message, "Syntax error: unrecognized escape sequence \\", &heap_stack);
							write_char(&error_message, cursor[1], &heap_stack);
							write(&error_message, "\n", &heap_stack);
							print(error_message);
							return nil;
						} break;
					}
					
					cursor += 2;
				} else if (*cursor == '\'') {
					increment_cursor();
					break;
				} else {
					write_char(&string_value, *cursor, &heap_stack);
					increment_cursor();
				}
				
				if (cursor >= source_end) {
					print_unexpected_end_of_file(string_start_site, "inside string.\n");
					return nil;
				}
			}
			
			auto new_token = push_new_token(TokenKind::string, string_start_site);
			new_token->string_value = string_value;
		}
		
		//anything else is considered an identifier character
		else {
			fox_assert(is_identifier_character(cursor));
			
			auto identifier_start_site = site_cursor;
			auto identifier_start = cursor;
			while (cursor < source_end && is_identifier_character(cursor)) {
				increment_cursor();
			}
			
			ConstString identifier;
			zero(&identifier);
			identifier.data = identifier_start;
			identifier.length = cursor - identifier_start;
			
			auto keyword_index_optional = find(Array<ConstString>(keyword_strings), identifier);
			if (keyword_index_optional) {
				auto new_token = push_new_token(TokenKind::keyword, identifier_start_site);
				new_token->keyword_value = (Keyword)keyword_index_optional.value;
			} else {
				auto new_token = push_new_token(TokenKind::identifier, identifier_start_site);
				new_token->identifier_value = identifier;
			}
		}
	}
	
	return tokens;
}

//---Backend types
struct CBackendValue {
	ConstString variable_name;
	u32 intermediate_id;
	b8 is_intermediate; //@todo maybe this should be replaced by a kind
};

internal CBackendValue c_backend_value_from_named_value(ConstString name) {
	CBackendValue backend_value;
	zero(&backend_value);
	backend_value.variable_name = name;
	return backend_value;
}

internal CBackendValue c_backend_value_from_intermediate_id(u32 intermediate_id) {
	CBackendValue backend_value;
	zero(&backend_value);
	backend_value.intermediate_id = intermediate_id;
	backend_value.is_intermediate = true;
	return backend_value;
}

//---Linearizer types
struct SyntaxNode;
struct SyntaxNodeFunction;

//@rename integer type? or do we want floats shoehorned in
struct AtomicType {
	u8 size; //size in bytes
	b8 is_signed;
};

internal bool is_equal(AtomicType atomic_type0, AtomicType atomic_type1) {
	return memcmp(&atomic_type0, &atomic_type1, sizeof(AtomicType)) == 0;
}

internal bool operator==(AtomicType atomic_type0, AtomicType atomic_type1) {
	return is_equal(atomic_type0, atomic_type1);
}

internal bool operator!=(AtomicType atomic_type0, AtomicType atomic_type1) {
	return !is_equal(atomic_type0, atomic_type1);
}

internal fuint size_in_bits(AtomicType atomic_type) {
	return atomic_type.size << 3; //Same as size * 2^3 or size * 8
}

template<typename AllocatorType>
internal void write(String* buffer, AtomicType atomic_type, AllocatorType* allocator) {
	if (atomic_type.is_signed) {
		write(buffer, "s", allocator);
	} else {
		write(buffer, "u", allocator);
	}
	write_uint(buffer, size_in_bits(atomic_type), allocator);
}

struct Value {
	CBackendValue backend_value;
	SourceSite* declaration_site;
	ConstString identifier;
	AtomicType type;
	b8 is_argument;
};

//---Parser
enum class SyntaxNodeKind : u8 {
	Invalid,
	UnaryOperation,
	BinaryOperation,
	ArrayAccess,
	Identifier,
	IntegerLiteral,
	StringLiteral,
	AtomicTypeNode,
	ArrayType,
	Function,
};

struct SyntaxNode {
	SourceSite* site;
	SyntaxNodeKind kind;
	Value* result;
};

struct SyntaxNodeUnaryOperation : SyntaxNode {
	Keyword operator_keyword;
	SyntaxNode* operand;
};

struct SyntaxNodeBinaryOperation : SyntaxNode {
	Keyword operator_keyword;
	SyntaxNode* operands[2];
};

struct SyntaxNodeIdentifier : SyntaxNode {
	ConstString identifier;
};

struct SyntaxNodeIntegerLiteral : SyntaxNode {
	u64 integer;
};

struct SyntaxNodeStringLiteral : SyntaxNode {
	ConstString string;
};

struct SyntaxNodeAtomicTypeNode : SyntaxNode {
	AtomicType atomic_type;
};

struct SyntaxNodeFunction : SyntaxNode {
	struct Argument {
		SourceSite* site; //site of argument name
		ConstString identifier;
		SyntaxNodeAtomicTypeNode* type;
	};
	
	ConstString identifier;
	Array<Argument> arguments;
	Array<SyntaxNodeAtomicTypeNode*> return_types;
	Array<SyntaxNode*> body;
	Array<SyntaxNode*> linear_syntax_nodes;
	Array<Value*> declarations;
};

struct ParseContext {
	Token* cursor;
	Token* tokens_end;
};

internal Optional<Array<SyntaxNode*>> parse(Array<Token> tokens);
internal Optional<Array<SyntaxNode*>> parse_statements(ParseContext* parser, bool inside_function = true);
internal SyntaxNodeFunction* parse_function(ParseContext* parser);
internal SyntaxNode* parse_expression(ParseContext* parser, u64 outer_precedence = 0);

template<typename AllocatorType> internal void write(String* buffer, Array<SyntaxNode*> syntax_nodes, fuint indent, AllocatorType* allocator);
template<typename AllocatorType> internal void write(String* buffer, SyntaxNode* syntax_node, fuint indent, AllocatorType* allocator);
template<typename AllocatorType> internal void write_function_opening(String* buffer, SyntaxNodeFunction* function, AllocatorType* allocator);
template<typename SyntaxNodeType> internal SyntaxNodeType* create_syntax_node_internal(SyntaxNodeKind kind, SourceSite* site);

template<typename SyntaxNodeType>
internal SyntaxNodeType* create_syntax_node_internal(SyntaxNodeKind kind, SourceSite* site) {
	auto syntax_node = allocate<SyntaxNodeType>(&heap_stack);
	syntax_node->kind = kind;
	syntax_node->site = site;
	return syntax_node;
}
#define create_syntax_node(type, site) create_syntax_node_internal<SyntaxNode##type>(SyntaxNodeKind::##type, site);

template<typename AllocatorType>
internal void write(String* buffer, Array<SyntaxNode*> syntax_nodes, fuint indent, AllocatorType* allocator) {
	fox_for (syntax_node_index, syntax_nodes.length) {
		write_indent(buffer, indent, allocator);
		write(buffer, syntax_nodes[syntax_node_index], indent, allocator);
		write(buffer, "\n", allocator);
	}
}

template<typename AllocatorType>
internal void write(String* buffer, SyntaxNode* syntax_node, fuint indent, AllocatorType* allocator) {
	switch (syntax_node->kind) {
		case SyntaxNodeKind::UnaryOperation: {
			auto unary_operation = (SyntaxNodeUnaryOperation*)syntax_node;
			write(buffer, "(", allocator);
			write(buffer, unary_operation->operator_keyword, allocator);
			write(buffer, " ", allocator);
			write(buffer, unary_operation->operand, indent, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case SyntaxNodeKind::BinaryOperation: {
			auto binary_operation = (SyntaxNodeBinaryOperation*)syntax_node;
			write(buffer, "(", allocator);
			write(buffer, binary_operation->operands[0], indent, allocator);
			write(buffer, " ", allocator);
			write(buffer, binary_operation->operator_keyword, allocator);
			write(buffer, " ", allocator);
			write(buffer, binary_operation->operands[1], indent, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case SyntaxNodeKind::Identifier: {
			auto identifier = (SyntaxNodeIdentifier*)syntax_node;
			write(buffer, identifier->identifier, allocator);
		} break;
		
		case SyntaxNodeKind::IntegerLiteral: {
			auto integer = (SyntaxNodeIntegerLiteral*)syntax_node;
			write_uint(buffer, integer->integer, allocator);
		} break;
		
		case SyntaxNodeKind::StringLiteral: {
			//@todo escape string literals so we don't have line endings
			auto string = (SyntaxNodeStringLiteral*)syntax_node;
			write(buffer, "'", allocator);
			write(buffer, string->string, allocator);
			write(buffer, "'", allocator);
		} break;
		
		case SyntaxNodeKind::Function: {
			auto function = (SyntaxNodeFunction*)syntax_node;
			write_function_opening(buffer, function, allocator);
			write(buffer, function->body, indent + 1, allocator);
			write_indent(buffer, indent, allocator);
			write(buffer, "}", allocator);
		} break;
		
		case SyntaxNodeKind::AtomicTypeNode: {
			auto atomic_type_node = (SyntaxNodeAtomicTypeNode*)syntax_node;
			write(buffer, atomic_type_node->atomic_type, allocator);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

template<typename AllocatorType>
internal void write_function_opening(String* buffer, SyntaxNodeFunction* function, AllocatorType* allocator) {
	write(buffer, "function ", allocator);
	write(buffer, function->identifier, allocator);
	write(buffer, "(", allocator);
	fox_for (argument_index, function->arguments.length) {
		auto argument = &function->arguments[argument_index];
		write(buffer, argument->identifier, allocator);
		write(buffer, ": ", allocator);
		write(buffer, argument->type->atomic_type, allocator);
		if (argument_index != function->arguments.length - 1) {
			write(buffer, ", ", allocator);
		}
	}
	write(buffer, "): int {\n", allocator);
}

internal bool expect_token_kind_internal(ParseContext* parser, TokenKind kind, ConstString where) {
	if (parser->cursor < parser->tokens_end) {
		if (parser->cursor->kind == kind) {
			return true;
		}
		
		String error_message;
		zero(&error_message);
		write(&error_message, "Syntax error: expected ", &heap_stack);
		
		ConstString kind_string;
		zero(&kind_string);
		switch (kind) {
			case TokenKind::keyword: {
				kind_string = "keyword";
			} break;
			
			case TokenKind::integer: {
				kind_string = "integer literal";
			} break;
			
			case TokenKind::string: {
				kind_string = "string literal";
			} break;
			
			case TokenKind::identifier: {
				kind_string = "identifier";
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
		
		write(&error_message, kind_string, &heap_stack);
		write(&error_message, " but got ", &heap_stack);
		write(&error_message, *parser->cursor, &heap_stack);
		write(&error_message, "\n", &heap_stack);
		print(error_message);
	} else {
		//@todo we should probably provide a different site for this
		print_unexpected_end_of_file(parser->cursor[-1].site, where);
	}
	
	return false;
}
#define expect_token_kind(parser, kind, where) if (!expect_token_kind_internal((parser), (kind), (where))) { return nullptr; }

internal bool expect_keyword_internal(ParseContext* parser, Keyword keyword, ConstString where) {
	if (expect_token_kind_internal(parser, TokenKind::keyword, where)) {
		if (parser->cursor->keyword_value == keyword) {
			parser->cursor++;
			return true;
		}
		
		String error_message;
		zero(&error_message);
		write(&error_message, parser->cursor->site, &heap_stack);
		write(&error_message, "Syntax error: expected keyword ", &heap_stack);
		write(&error_message, keyword, &heap_stack);
		write(&error_message, "\n", &heap_stack);
		print(error_message);
	}
	
	return false;
}
#define expect_keyword(parser, keyword, where) if (!expect_keyword_internal((parser), (keyword), (where))) { return nullptr; }

internal Optional<Array<SyntaxNode*>> parse(Array<Token> tokens) {
	fox_assert(tokens);
	
	ParseContext parser;
	zero(&parser);
	parser.cursor = tokens.data;
	parser.tokens_end = tokens.data + tokens.length;
	
	return parse_statements(&parser, false);
}

internal Optional<Array<SyntaxNode*>> parse_statements(ParseContext* parser, bool inside_function) {
	Array<SyntaxNode*> statements;
	zero(&statements);
	
	while (parser->cursor < parser->tokens_end) {
		if (parser->cursor->kind == TokenKind::keyword) {
			switch (parser->cursor->keyword_value) {
				case Keyword::function: {
					auto function = parse_function(parser);
					if (function) {
						push(&statements, function, &heap_stack);
					} else {
						return nil;
					}
				} break;
				
				case Keyword::return_keyword: {
					if (inside_function) {
						auto return_site = &parser->cursor->site;
						parser->cursor++;
						auto return_expression = parse_expression(parser);
						if (return_expression) {
							auto return_operation = create_syntax_node(UnaryOperation, return_site);
							return_operation->operator_keyword = Keyword::return_keyword;
							return_operation->operand = return_expression;
							push(&statements, return_operation, &heap_stack);
						} else {
							return nil;
						}
					} else {
						String error_message;
						zero(&error_message);
						write(&error_message, parser->cursor->site, &heap_stack);
						write(&error_message, "Syntax error: return is only allowed inside a function.\n", &heap_stack);
						print(error_message);
						return nil;
					}
				} break;
				
				case Keyword::close_curly: {
					if (inside_function) {
						return statements;
					} else {
						String error_message;
						zero(&error_message);
						write(&error_message, parser->cursor->site, &heap_stack);
						write(&error_message, "Syntax error: stray close curly brace (}).\n", &heap_stack);
						print(error_message);
						return nil;
					}
				} break;
				
				default: {
					String error_message;
					zero(&error_message);
					write(&error_message, "Syntax error: unexpected keyword at top level ", &heap_stack);
					write(&error_message, parser->cursor->keyword_value, &heap_stack);
					write(&error_message, "\n", &heap_stack);
					print(error_message);
					return nil;
				} break;
			}
		} else {
			//@todo allow global declarations
			if (inside_function) {
				auto expression = parse_expression(parser);
				if (expression) {
					push(&statements, expression, &heap_stack);
				} else {
					return nil;
				}
			} else {
				String error_message;
				zero(&error_message);
				write(&error_message, parser->cursor->site, &heap_stack);
				write(&error_message, "Syntax error: expression outside of function.\n", &heap_stack);
				print(error_message);
				return nil;
			}
		}
	}
	
	return statements;
}

internal SyntaxNodeAtomicTypeNode* parse_type(ParseContext* parser) {
	AtomicType atomic_type;
	zero(&atomic_type);
	
	if (is_integer_type_keyword(*parser->cursor)) {
		auto integer_keyword = parser->cursor->keyword_value;
		auto is_signed = is_signed_integer_type_keyword(integer_keyword);
		if (integer_keyword == Keyword::integer_int || integer_keyword == Keyword::integer_uint) {
			atomic_type.size = 8;
			atomic_type.is_signed = is_signed;
		} else {
			auto first_integer_keyword = is_signed ? Keyword::integer_s8 : Keyword::integer_u8;
			auto size_index = (u8)integer_keyword - (u8)first_integer_keyword;
			atomic_type.size = 1 << size_index;
		}
	} else {
		String error_message;
		zero(&error_message);
		write(&error_message, parser->cursor->site, &heap_stack);
		write(&error_message, "Syntax error: expected integer type.\n", &heap_stack);
		print(error_message);
		return nullptr;
	}
	
	auto atomic_type_node = create_syntax_node(AtomicTypeNode, &parser->cursor->site);
	atomic_type_node->atomic_type = atomic_type;
	
	parser->cursor++;
	return atomic_type_node;
}

internal SyntaxNodeFunction* parse_function(ParseContext* parser) {
	//function name(argument1: int, argument2: int,): int {}
	//function name() {}
	fox_assert(parser->cursor->kind == TokenKind::keyword && parser->cursor->keyword_value == Keyword::function);
	auto function_site = &parser->cursor->site;
	parser->cursor++;
	
	expect_token_kind(parser, TokenKind::identifier, "in function declaration, expected function name.\n");
	auto function_identifier = parser->cursor->identifier_value;
	parser->cursor++;
	
	expect_keyword(parser, Keyword::open_parenthesis, "in function declaration, expected open parenthesis.\n");
	
	//Parse arguments
	Array<SyntaxNodeFunction::Argument> arguments;
	zero(&arguments);
	while (true) {
		if (parser->cursor >= parser->tokens_end) {
			print_unexpected_end_of_file(parser->cursor[-1].site, "in function declaration, expected close parenthesis or next argument.\n");
			return nullptr;
		}
		
		if (parser->cursor->kind == TokenKind::keyword) {
			expect_keyword(parser, Keyword::close_parenthesis, "in function declaration, expected close parenthesis or next argument.\n");
			break;
		}
		
		expect_token_kind(parser, TokenKind::identifier, "in function declaration, expected name of argument.\n");
		auto argument_site = &parser->cursor->site;
		auto argument_identifier = parser->cursor->identifier_value;
		parser->cursor++;
		expect_keyword(parser, Keyword::declare, "in function declaration, expected type of argumnet.\n");
		auto argument_type = parse_type(parser);
		
		auto new_argument = push_zero(&arguments, &heap_stack);
		new_argument->site = argument_site;
		new_argument->identifier = argument_identifier;
		new_argument->type = argument_type;
		
		expect_token_kind(parser, TokenKind::keyword, "in function declaration, expected close parenthesis or next argument.\n");
		if (!(parser->cursor->keyword_value == Keyword::comma || parser->cursor->keyword_value == Keyword::close_parenthesis)) {
			String error_message;
			zero(&error_message);
			write(&error_message, "Syntax error: expected comma or close parenthesis but got ", &heap_stack);
			write(&error_message, *parser->cursor, &heap_stack);
			write(&error_message, "\n", &heap_stack);
			print(error_message);
			return nullptr;
		}
		
		if (parser->cursor->keyword_value == Keyword::comma) {
			parser->cursor++;
		}
	}
	
	expect_token_kind(parser, TokenKind::keyword, "in function declaration, expected return value or function body.\n");
	
	Array<SyntaxNodeAtomicTypeNode*> return_types;
	zero(&return_types);
	if (parser->cursor->keyword_value == Keyword::declare) {
		parser->cursor++;
		
		while (true) {
			auto return_type = parse_type(parser);
			push(&return_types, return_type, &heap_stack);
			
			expect_token_kind(parser, TokenKind::keyword, "in function declaration, expected next return value or function body.\n");
			if (parser->cursor->keyword_value == Keyword::open_curly) {
				break;
			}
			expect_keyword(parser, Keyword::comma, "in function declaration, expected next return value or function body.\n");
			parser->cursor++;
		}
	}
	
	//@todo clean this up a bit, function x() , int {} will tell us we expected function body at comma
	expect_keyword(parser, Keyword::open_curly, "in function declaration, expected function body.\n");
	auto function_body = parse_statements(parser);
	if (function_body) {
		expect_keyword(parser, Keyword::close_curly, "in function declaration, expected end of function body.\n");
		
		auto function = create_syntax_node(Function, function_site);
		function->identifier = function_identifier;
		function->arguments = arguments;
		function->return_types = return_types;
		function->body = function_body.value;
		return function;
	}
	
	return nullptr;
}

internal SyntaxNode* parse_expression(ParseContext* parser, u64 outer_precedence) {
	fox_assert(parser->cursor < parser->tokens_end);
	
	SyntaxNode* left = nullptr;
	switch (parser->cursor->kind) {
		case TokenKind::identifier: {
			auto identifier = create_syntax_node(Identifier, &parser->cursor->site);
			identifier->identifier = parser->cursor->identifier_value;
			left = identifier;
		} break;
		
		case TokenKind::integer: {
			auto integer = create_syntax_node(IntegerLiteral, &parser->cursor->site);
			integer->integer = parser->cursor->integer_value;
			left = integer;
		} break;
		
		case TokenKind::string: {
			auto string = create_syntax_node(StringLiteral, &parser->cursor->site);
			string->string = parser->cursor->string_value;
			left = string;
		} break;
		
		case TokenKind::keyword: {
			String error_message;
			zero(&error_message);
			write(&error_message, parser->cursor->site, &heap_stack);
			write(&error_message, "Syntax error: unexpected keyword in expression.\n", &heap_stack);
			print(error_message);
			return nullptr;
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
	
	parser->cursor++;
	while (parser->cursor < parser->tokens_end
		   && parser->cursor->kind == TokenKind::keyword
		   && is_binary_operator(parser->cursor->keyword_value)) {
		auto operator_keyword = parser->cursor->keyword_value;
		auto precedence = precedences[(fuint)operator_keyword];
		if (precedence > outer_precedence || (precedence == outer_precedence && is_right_associative(operator_keyword))) {
			//Currently we only have binary operators
			auto binary_operation_site = &parser->cursor->site;
			auto binary_operator = create_syntax_node(BinaryOperation, binary_operation_site);
			binary_operator->operator_keyword = parser->cursor->keyword_value;
			binary_operator->operands[0] = left;
			
			parser->cursor++;
			if (parser->cursor < parser->tokens_end) {
				SyntaxNode* right = nullptr;
				if (binary_operator->operator_keyword == Keyword::declare) {
					right = parse_type(parser);
				} else {
					right = parse_expression(parser, precedence);
				}
				
				if (right) {
					binary_operator->operands[1] = right;
					left = binary_operator;
				} else {
					return nullptr;
				}
			} else {
				print_unexpected_end_of_file(*binary_operation_site, "after binary operator.\n");
				return nullptr;
			}
		} else {
			break;
		}
	}
	
	return left;
}

//---Linearizer context
struct LinearizerContext {
	Array<SyntaxNodeFunction*> functions;
};

//---Linearizer
internal bool linearize_function(LinearizerContext* linearizer, SyntaxNodeFunction* function);
internal bool linearize_syntax_node(LinearizerContext* linearizer, SyntaxNode* node, SyntaxNodeFunction* containing_function, bool is_statement = false);
internal void push_syntax_node(SyntaxNode* node, SyntaxNodeFunction* containing_function);
internal void print_statement_does_nothing_warning(SyntaxNode* statement);

internal bool linearize(LinearizerContext* linearizer, Array<SyntaxNode*> statements) {
	fox_for (statement_index, statements.length) {
		auto statement = (SyntaxNodeFunction*)statements[statement_index];
		fox_assert(statement->kind == SyntaxNodeKind::Function);
		if (!linearize_function(linearizer, statement)) {
			return false;
		}
	}
	
	return true;
}

internal bool linearize_function(LinearizerContext* linearizer, SyntaxNodeFunction* function) {
	push(&linearizer->functions, function, &heap_stack);
	
	fox_for (argument_index, function->arguments.length) {
		auto argument = &function->arguments[argument_index];
		auto argument_value = allocate<Value>(&heap_stack);
		argument_value->identifier = argument->identifier;
		argument_value->type = argument->type->atomic_type;
		argument_value->declaration_site = argument->site;
		argument_value->is_argument = true;
		push(&function->declarations, argument_value, &heap_stack);
	}
	
	fox_for (statement_index, function->body.length) {
		if (!linearize_syntax_node(linearizer, function->body[statement_index], function, true)) {
			return false;
		}
	}
	
	return true;
}

internal bool linearize_syntax_node(LinearizerContext* linearizer, SyntaxNode* syntax_node, SyntaxNodeFunction* containing_function, bool is_statement) {
	switch (syntax_node->kind) {
		case SyntaxNodeKind::UnaryOperation: {
			auto unary_operation = (SyntaxNodeUnaryOperation*)syntax_node;
			fox_assert(unary_operation->operator_keyword == Keyword::return_keyword);
			if (!linearize_syntax_node(linearizer, unary_operation->operand, containing_function)) {
				return false;
			}
			
			push_syntax_node(unary_operation, containing_function);
		} break;
		
		case SyntaxNodeKind::BinaryOperation: {
			auto binary_operation = (SyntaxNodeBinaryOperation*)syntax_node;
			if (binary_operation->operator_keyword == Keyword::declare) {
				auto declaration_identifier = (SyntaxNodeIdentifier*)binary_operation->operands[0];
				if (declaration_identifier->kind != SyntaxNodeKind::Identifier) {
					String error_message;
					zero(&error_message);
					write(&error_message, *declaration_identifier->site, &heap_stack);
					write(&error_message, "Syntax Error: expected identifier in declaration.\n", &heap_stack);
					print(error_message);
					return false;
				}
				
				fox_for (value_index, containing_function->declarations.length) {
					auto value = containing_function->declarations[value_index];
					if (value->identifier == declaration_identifier->identifier) {
						String error_message;
						zero(&error_message);
						write(&error_message, *binary_operation->site, &heap_stack);
						write(&error_message, "Syntax Error: redeclaration of previously declared value.\n", &heap_stack);
						write(&error_message, *value->declaration_site, &heap_stack);
						write(&error_message, "Note: previous declaration is here.\n", &heap_stack);
						print(error_message);
						return false;
					}
				}
				
				auto new_value = allocate<Value>(&heap_stack);
				new_value->identifier = declaration_identifier->identifier;
				push(&containing_function->declarations, new_value, &heap_stack);
				
				binary_operation->result = new_value;
			} else {
				if (!(linearize_syntax_node(linearizer, binary_operation->operands[0], containing_function)
					  && linearize_syntax_node(linearizer, binary_operation->operands[1], containing_function))) {
					return false;
				}
			}
			
			push_syntax_node(binary_operation, containing_function);
		} break;
		
		case SyntaxNodeKind::Identifier:
		case SyntaxNodeKind::IntegerLiteral:
		case SyntaxNodeKind::StringLiteral: {
			if (is_statement) {
				print_statement_does_nothing_warning(syntax_node);
			}
			
			push_syntax_node(syntax_node, containing_function);
		} break;
		
		case SyntaxNodeKind::Function: {
			auto function_node = (SyntaxNodeFunction*)syntax_node;
			if (!linearize_function(linearizer, function_node)) {
				return false;
			}
		} break;
		
		case SyntaxNodeKind::AtomicTypeNode:
		default: {
			fox_unreachable;
		} break;
	}
	
	return true;
}

internal void push_syntax_node(SyntaxNode* syntax_node, SyntaxNodeFunction* containing_function) {
	push(&containing_function->linear_syntax_nodes, syntax_node, &heap_stack);
}

internal void print_statement_does_nothing_warning(SyntaxNode* statement) {
	auto restore_point = create_restore_point(&heap_stack);
	String error_message;
	zero(&error_message);
	write(&error_message, *statement->site, &heap_stack);
	write(&error_message, "Warning: statement does nothing.\n", &heap_stack);
	print(error_message);
	restore(&heap_stack, restore_point);
}

template<typename AllocatorType>
internal void write(String* buffer, LinearizerContext* linearizer, AllocatorType* allocator) {
	fox_for (function_index, linearizer->functions.length) {
		auto function = linearizer->functions[function_index];
		write_function_opening(buffer, function, allocator);
		write(buffer, function->linear_syntax_nodes, 1, allocator);
		write(buffer, "}\n\n", allocator);
	}
}

//---Validater
internal bool validate_syntax_node_semantics(SyntaxNode* syntax_node, SyntaxNodeFunction* containing_function) {
	switch (syntax_node->kind) {
		case SyntaxNodeKind::UnaryOperation: {
			auto unary_operation = (SyntaxNodeUnaryOperation*)syntax_node;
			fox_assert(unary_operation->operator_keyword == Keyword::return_keyword);
			fox_assert(containing_function->return_types.length == 1); //@todo @bug support multiple returns
			if (unary_operation->operand->result->type != containing_function->return_types[0]->atomic_type) {
				String error_message;
				zero(&error_message);
				write(&error_message, *unary_operation->operand->site, &heap_stack);
				write(&error_message, "Syntax Error: expected integer in return statement.\n", &heap_stack);
				print(error_message);
				return false;
			}
		} break;
		
		case SyntaxNodeKind::BinaryOperation: {
			auto binary_operation = (SyntaxNodeBinaryOperation*)syntax_node;
			
			if (binary_operation->operator_keyword == Keyword::declare) {
				auto type = (SyntaxNodeAtomicTypeNode*)binary_operation->operands[1];
				fox_assert(type->kind == SyntaxNodeKind::AtomicTypeNode);
				
				//result is created during linearization which handles the actual declaration aspect
				binary_operation->result->type = type->atomic_type;
			} else if (binary_operation->operator_keyword == Keyword::assign) {
				//@bug we are not checking if the left side is an l-value
				if (binary_operation->operands[0]->result->type != binary_operation->operands[1]->result->type) {
					String error_message;
					zero(&error_message);
					write(&error_message, *binary_operation->site, &heap_stack);
					write(&error_message, "Type error: cannot assign ", &heap_stack);
					write(&error_message, binary_operation->operands[1]->result->type, &heap_stack);
					write(&error_message, " to ", &heap_stack);
					write(&error_message, binary_operation->operands[0]->result->type, &heap_stack);
					write(&error_message, "\n", &heap_stack);
					print(error_message);
					return false;
				}
				
				syntax_node->result = allocate<Value>(&heap_stack);
				syntax_node->result->type = binary_operation->operands[0]->result->type;
			} else {
				//Arithmetic operator
				auto operands = binary_operation->operands;
				if (operands[0]->result->type != operands[1]->result->type) {
					String error_message;
					zero(&error_message);
					write(&error_message, *binary_operation->site, &heap_stack);
					write(&error_message, "Type error: operands must be same type.\n", &heap_stack);
					write(&error_message, "Types are ", &heap_stack);
					write(&error_message, operands[0]->result->type, &heap_stack);
					write(&error_message, " and ", &heap_stack);
					write(&error_message, operands[1]->result->type, &heap_stack);
					write(&error_message, "\n", &heap_stack);
					print(error_message);
					return false;
				}
				
#if 0
				fox_for (operand_index, 2) {
					auto operand = binary_operation->operands[operand_index];
					if (operand->result->type != AtomicType::integer) {
						String error_message;
						zero(&error_message);
						write(&error_message, *operand->site, &heap_stack);
						write(&error_message, "Type error: operand of binary operation must be integers.\n", &heap_stack);
						write(&error_message, "Type is ", &heap_stack);
						write(&error_message, operand->result->type, &heap_stack);
						write(&error_message, "\n", &heap_stack);
						print(error_message);
						return false;
					}
				}
#endif
				
				syntax_node->result = allocate<Value>(&heap_stack);
				syntax_node->result->type = operands[0]->result->type;
			}
		} break;
		
		case SyntaxNodeKind::Identifier: {
			auto identifier = ((SyntaxNodeIdentifier*)syntax_node)->identifier;
			Value* identified_value = nullptr;
			fox_for (declaration_index, containing_function->declarations.length) {
				auto declaration = containing_function->declarations[declaration_index];
				if (declaration->identifier == identifier) {
					identified_value = declaration;
					break;
				}
			}
			
			if (!identified_value) {
				String error_message;
				zero(&error_message);
				write(&error_message, *syntax_node->site, &heap_stack);
				write(&error_message, "Semantic error: undeclared identifier ", &heap_stack);
				write(&error_message, identifier, &heap_stack);
				write(&error_message, "\n", &heap_stack);
				print(error_message);
				return false;
			}
			
			syntax_node->result = identified_value;
		} break;
		
		case SyntaxNodeKind::IntegerLiteral: {
			syntax_node->result = allocate<Value>(&heap_stack);
			syntax_node->result->type.is_signed = true;
			syntax_node->result->type.size = 8;
		} break;
		
		case SyntaxNodeKind::StringLiteral: //@todo
		case SyntaxNodeKind::AtomicTypeNode:
		case SyntaxNodeKind::Function:
		default: {
			fox_unreachable;
		}
	}
	
	return true;
}

internal bool validate_function_semantics(SyntaxNodeFunction* function) {
	fox_for (syntax_node_index, function->linear_syntax_nodes.length) {
		if (!validate_syntax_node_semantics(function->linear_syntax_nodes[syntax_node_index], function)) {
			return false;
		}
	}
	
	//Check for unreachable code
	auto return_found = false;
	fox_for (statement_index, function->body.length - 1) {
		auto maybe_return_statement = (SyntaxNodeUnaryOperation*)function->body[statement_index];
		if (maybe_return_statement->kind == SyntaxNodeKind::UnaryOperation) {
			fox_assert(maybe_return_statement->operator_keyword == Keyword::return_keyword);
			return_found = true;
			auto next_statement = function->body[statement_index + 1];
			String error_message;
			zero(&error_message);
			write(&error_message, *next_statement->site, &heap_stack);
			write(&error_message, "Warning: unreachable code.\n", &heap_stack);
			print(error_message);
		}
	}
	
	if (!return_found) {
		//Check for terminating return
		auto last_statement = (SyntaxNodeUnaryOperation*)function->body[function->body.length - 1];
		if (last_statement->kind != SyntaxNodeKind::UnaryOperation
			|| last_statement->operator_keyword != Keyword::return_keyword) {
			String error_message;
			zero(&error_message);
			write(&error_message, *last_statement->site, &heap_stack);
			write(&error_message, "Semantic error: function should end with return statement.\n", &heap_stack);
			print(error_message);
			return false;
		}
	}
	
	return true;
}

//Semantic validation is responsible for determining the types of expressions and statements
//and ensuring the rules of the language are upheld
internal bool validate_semantics(LinearizerContext* linearizer) {
	fox_for (function_index, linearizer->functions.length) {
		auto function = linearizer->functions[function_index];
		if (!validate_function_semantics(function)) {
			return false;
		}
	}
	
	return true;
}

//---C backend
internal void compile_intermediate_constant_name_c(String* c_code, u32 intermediate_id) {
	fox_assert(intermediate_id);
	write(c_code, "__VI_i", &heap_stack);
	write_uint(c_code, intermediate_id, &heap_stack);
}

internal void compile_value_c(String* c_code, CBackendValue backend_value) {
	if (backend_value.is_intermediate) {
		compile_intermediate_constant_name_c(c_code, backend_value.intermediate_id);
	} else {
		fox_assert(backend_value.variable_name);
		write(c_code, backend_value.variable_name, &heap_stack);
	}
}

internal void compile_value_c(String* c_code, Value* value) {
	compile_value_c(c_code, value->backend_value);
}

internal void compile_type_c(String* c_code, AtomicType type) {
	if (!type.is_signed) {
		write(c_code, "u", &heap_stack);
	}
	write(c_code, "int", &heap_stack);
	write_uint(c_code, size_in_bits(type), &heap_stack);
	write(c_code, "_t", &heap_stack);
}

internal void compile_intermediate_value_open_c(String* c_code, u32 intermediate_id, AtomicType intermediate_type) {
	compile_type_c(c_code, intermediate_type);
	write(c_code, " ", &heap_stack);
	compile_intermediate_constant_name_c(c_code, intermediate_id);
	write(c_code, " = ", &heap_stack);
}

internal void compile_intermediate_value_open_c(String* c_code, CBackendValue backend_value, AtomicType intermediate_type) {
	fox_assert(backend_value.is_intermediate);
	compile_intermediate_value_open_c(c_code, backend_value.intermediate_id, intermediate_type);
}

internal void compile_intermediate_value_open_c(String* c_code, Value* value) {
	compile_intermediate_value_open_c(c_code, value->backend_value, value->type);
}

internal void compile_intermediate_value_close_c(String* c_code) {
	write(c_code, ";\n", &heap_stack);
}

internal void compile_signature_c(String* c_code, SyntaxNodeFunction* function) {
	//@todo @bug support multiple returns
	fox_assert(function->return_types.length == 1);
	
	compile_type_c(c_code, function->return_types[0]->atomic_type);
	write(c_code, " ", &heap_stack);
	write(c_code, function->identifier, &heap_stack);
	write(c_code, "(", &heap_stack);
	
	fox_for (argument_index, function->arguments.length) {
		auto argument = &function->arguments[argument_index];
		compile_type_c(c_code, argument->type->atomic_type);
		write(c_code, " ", &heap_stack);
		write(c_code, argument->identifier, &heap_stack);
		if (argument_index != function->arguments.length - 1) {
			write(c_code, ", ", &heap_stack);
		}
	}
	
	write(c_code, ")", &heap_stack);
}

internal void compile_forward_declaration_c(String* c_code, SyntaxNodeFunction* function) {
	compile_signature_c(c_code, function);
	write(c_code, ";\n", &heap_stack);
}

internal void compile_function_c(String* c_code, SyntaxNodeFunction* function) {
	compile_signature_c(c_code, function);
	write(c_code, " {\n", &heap_stack);
	
	fox_for (declaration_index, function->declarations.length) {
		auto declaration = function->declarations[declaration_index];
		declaration->backend_value = c_backend_value_from_named_value(declaration->identifier);
		if (!declaration->is_argument) {
			write_indent(c_code, 1, &heap_stack);
			compile_type_c(c_code, declaration->type);
			write(c_code, " ", &heap_stack);
			write(c_code, declaration->identifier, &heap_stack);
			write(c_code, " = 0;\n", &heap_stack);
		}
	}
	
	u32 intermediate_id_cursor = 0;
	
	fox_for (syntax_node_index, function->linear_syntax_nodes.length) {
		auto syntax_node = function->linear_syntax_nodes[syntax_node_index];
		switch (syntax_node->kind) {
			case SyntaxNodeKind::UnaryOperation: {
				auto unary_operation = (SyntaxNodeUnaryOperation*)syntax_node;
				fox_assert(unary_operation->operator_keyword == Keyword::return_keyword);
				write_indent(c_code, 1, &heap_stack);
				write(c_code, "return ", &heap_stack);
				compile_value_c(c_code, unary_operation->operand->result);
				write(c_code, ";\n", &heap_stack);
			} break;
			
			case SyntaxNodeKind::BinaryOperation: {
				auto binary_operation = (SyntaxNodeBinaryOperation*)syntax_node;
				if (binary_operation->operator_keyword != Keyword::declare) {
					binary_operation->result->backend_value = c_backend_value_from_intermediate_id(++intermediate_id_cursor);
					write_indent(c_code, 1, &heap_stack);
					compile_intermediate_value_open_c(c_code, binary_operation->result);
					compile_value_c(c_code, binary_operation->operands[0]->result);
					write(c_code, " ", &heap_stack);
					write(c_code, keyword_strings[(fuint)binary_operation->operator_keyword], &heap_stack);
					write(c_code, " ", &heap_stack);
					compile_value_c(c_code, binary_operation->operands[1]->result);
					compile_intermediate_value_close_c(c_code);
				} //else do nothing since declarations are compiled at the start of the scope
			} break;
			
			case SyntaxNodeKind::IntegerLiteral: {
				auto integer_literal = (SyntaxNodeIntegerLiteral*)syntax_node;
				integer_literal->result->backend_value = c_backend_value_from_intermediate_id(++intermediate_id_cursor);
				write_indent(c_code, 1, &heap_stack);
				compile_intermediate_value_open_c(c_code, integer_literal->result);
				write_uint(c_code, integer_literal->integer, &heap_stack);
				compile_intermediate_value_close_c(c_code);
			} break;
			
			case SyntaxNodeKind::Identifier:
			case SyntaxNodeKind::StringLiteral: 
			case SyntaxNodeKind::AtomicTypeNode:
			case SyntaxNodeKind::Function: {
				//Do nothing
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
	}
	
	write(c_code, "}\n", &heap_stack);
}

internal String compile_c(LinearizerContext* linearizer) {
	String c_code;
	zero(&c_code);
	
	fox_for (function_index, linearizer->functions.length) {
		compile_forward_declaration_c(&c_code, linearizer->functions[function_index]);
	}
	
	fox_for (function_index, linearizer->functions.length) {
		compile_function_c(&c_code, linearizer->functions[function_index]);
	}
	
	return c_code;
}

//---Interpreter
internal void interpret(String file_string, ConstString file_path) {
	auto maybe_tokens = lex(file_string, file_path);
	fox_assert(maybe_tokens);
	auto tokens = maybe_tokens.value;
	
#if enable_lexer_print
	String tokenized_file;
	zero(&tokenized_file);
	write(&tokenized_file, "----------Lexer Output----------\n", &heap_stack);
	fox_for (itoken, tokens.length) {
		write(&tokenized_file, tokens[itoken], &heap_stack);
		write(&tokenized_file, "\n", &heap_stack);
		write(&tokenized_file, tokens[itoken].site, &heap_stack);
		write(&tokenized_file, "\n", &heap_stack);
	}
	write(&tokenized_file, "\n\n", &heap_stack);
	print(tokenized_file);
#endif
	
	auto maybe_syntax_tree = parse(tokens);
	if (maybe_syntax_tree) {
		auto syntax_tree = maybe_syntax_tree.value;
		
#if enable_parser_print
		String syntax_tree_string;
		zero(&syntax_tree_string);
		write(&syntax_tree_string, "\n\n----------Parser Output----------\n", &heap_stack);
		write(&syntax_tree_string, syntax_tree, 0, &heap_stack);
		write(&syntax_tree_string, "\n\n", &heap_stack);
		print(syntax_tree_string);
#endif
		
		LinearizerContext linearizer;
		zero(&linearizer);
		auto linearize_successful = linearize(&linearizer, syntax_tree);
		fox_assert(linearize_successful);
		
#if enable_linearizer_print
		String linearizer_string;
		zero(&linearizer_string);
		write(&linearizer_string, "\n\n----------Linearizer Output----------\n", &heap_stack);
		write(&linearizer_string, &linearizer, &heap_stack);
		write(&linearizer_string, "\n\n", &heap_stack);
		print(linearizer_string);
#endif
		
		if (validate_semantics(&linearizer)) {
			auto c_code = compile_c(&linearizer);
#if enable_c_backend_print
			String c_code_console_output;
			zero(&c_code_console_output);
			write(&c_code_console_output, "\n\n----------C Backend Output----------\n", &heap_stack);
			write(&c_code_console_output, c_code, &heap_stack);
			write(&c_code_console_output, "\n\n", &heap_stack);
			print(c_code_console_output);
#endif
		}
	}
}

int main(int argument_count, char** arguments) {
	test_foxlib();
	
	u64 memory_size = mebibytes(1);
	void* memory_block = allocate_large_block_fixed_address_system(nullptr, memory_size);
	fox_assert(memory_block);
	initialize(&heap_stack, memory_block, memory_size);
	
	if (argument_count > 1) {
		ConstString file_path = arguments[1];
		Array<u8> file_data;
		zero(&file_data);
		auto file_read_status = read_entire_file(file_path.data, &heap_stack, &file_data);
		switch (file_read_status) {
			case FileStatus::read: {
				auto file_string = fox_interpret_cast(String, file_data);
				interpret(file_string, file_path);
			} break;
			
			case FileStatus::empty: {
				String error_message;
				zero(&error_message);
				write(&error_message, "File ", &heap_stack);
				write(&error_message, file_path, &heap_stack);
				write(&error_message, " is empty!\n", &heap_stack);
				print(error_message);
			} break;
			
			case FileStatus::error: {
				String error_message;
				zero(&error_message);
				write(&error_message, "File ", &heap_stack);
				write(&error_message, file_path, &heap_stack);
				write(&error_message, " is inaccessible!\n", &heap_stack);
				print(error_message);
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
	} else {
		print("Usage: vulpes <file to compile>\n");
	}
	
	return 0;
}