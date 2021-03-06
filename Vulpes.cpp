//---Program Options
#define enable_lexer_print 1
#define enable_parser_print 1
#define enable_linearizer_print 1
#define enable_c_backend_print 0

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
	if_keyword,
	else_keyword,
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
	"if",
	"else",
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
	0,   // if
	0,   // else
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

internal bool is_signed_integer_type_keyword(Keyword integer_keyword) {
	fox_assert(is_integer_type_keyword(integer_keyword));
	return integer_keyword >= Keyword::integer_s8;
}

internal u8 get_size_of_integer_type_keyword(Keyword integer_keyword, bool is_signed) {
	fox_assert(is_integer_type_keyword(integer_keyword));
	fox_assert(!is_signed || is_signed_integer_type_keyword(integer_keyword));
	
	if (integer_keyword == Keyword::integer_int || integer_keyword == Keyword::integer_uint) {
		return 8;
	}
	
	auto first_integer_keyword = is_signed ? Keyword::integer_s8 : Keyword::integer_u8;
	auto size_index = (u8)integer_keyword - (u8)first_integer_keyword;
	auto size = (u8)(1 << size_index);
	return size;
}

internal bool is_binary_operator(Keyword keyword) {
	return Keyword::assign <= keyword && keyword <= Keyword::subtract;
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
				
				u64 integer_value = unsigned_integer_from_string(number_string, 10);
				
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

//---Type system
enum class TypeKind {
	Invalid,
	Atomic,
	FixedLengthArray,
};

struct Type {
	TypeKind kind;
};

//@rename integer type? or do we want floats shoehorned in
struct TypeAtomic : Type {
	u8 size; //size in bytes
	b8 is_signed;
};

struct TypeFixedLengthArray : Type {
	u64 length;
	Type* element_type;
};

template<typename TypeType>
internal TypeType* create_type_internal(TypeKind kind) {
	auto new_type = allocate<TypeType>(&heap_stack);
	new_type->kind = kind;
	return new_type;
}
#define create_type(type_type) create_type_internal<Type##type_type>(TypeKind::##type_type)

internal u64 get_size(Type* type) {
	switch (type->kind) {
		case TypeKind::Atomic: {
			auto atomic_type = (TypeAtomic*)type;
			return atomic_type->size;
		} break;
		
		case TypeKind::FixedLengthArray: {
			auto array_type = (TypeFixedLengthArray*)type;
			auto element_size = get_size(array_type->element_type);
			return element_size * array_type->length;
		} break;
	}
	
	fox_unreachable;
	return 0;
}

internal bool is_equal(Type* type0, Type* type1) {
	if (type0->kind == type1->kind) {
		switch (type0->kind) {
			case TypeKind::Atomic: {
				return memcmp(type0, type1, sizeof(TypeAtomic)) == 0;
			} break;
			
			case TypeKind::FixedLengthArray: {
				auto array_type0 = (TypeFixedLengthArray*)type0;
				auto array_type1 = (TypeFixedLengthArray*)type1;
				return array_type0->length == array_type1->length
					&& is_equal(array_type0->element_type, array_type0->element_type);
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
	}
	
	return false;
}

internal bool operator==(Type& type0, Type& type1) {
	return is_equal(&type0, &type1);
}

internal bool operator!=(Type& type0, Type& type1) {
	return !is_equal(&type0, &type1);
}

internal fuint size_in_bits(TypeAtomic atomic_type) {
	return atomic_type.size << 3; //Same as size * 2^3 or size * 8
}

template<typename AllocatorType>
internal void write(String* buffer, Type* type, AllocatorType* allocator) {
	switch (type->kind) {
		case TypeKind::Atomic: {
			auto atomic_type = (TypeAtomic*)type;
			if (atomic_type->is_signed) {
				write(buffer, "s", allocator);
			} else {
				write(buffer, "u", allocator);
			}
			write_uint(buffer, size_in_bits(*atomic_type), allocator);
		} break;
		
		case TypeKind::FixedLengthArray: {
			auto array_type = (TypeFixedLengthArray*)type;
			write(buffer, "[", allocator);
			write_uint(buffer, array_type->length, allocator);
			write(buffer, "]", allocator);
			write(buffer, array_type->element_type, allocator);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

//---Linearizer types
struct SyntaxNode;
struct SyntaxNodeFunction;
struct SyntaxNodeIfElse;
struct ControlFlowNode;

struct Value {
	CBackendValue backend_value;
	SourceSite* declaration_site;
	ConstString identifier;
	Type* type;
	b8 is_argument;
};

struct LinearizedType {
	Type** result;
	Array<SyntaxNode*> type_nodes;
};

struct LinearizerContext {
	Array<SyntaxNodeFunction*> functions;
	Array<LinearizedType*> linearized_types;
};

struct ScopedStatements {
	SyntaxNodeFunction* containing_function;
	ScopedStatements* outer_scope;
	
	Array<SyntaxNode*> statements;
	Array<Value*> declarations;
};

enum class ControlFlowNodeKind {
	Invalid,
	Unconditional,
	IfElse,
};

//Syntax nodes in control flow graphs are always in dependency order
//i.e. x = y + z would be stored as
//x
//y
//z
//y + z
//x = y + z
//they are guaranteed not to terminate in the middle of a block
//they must either termiante at the end or not at all
struct ControlFlowNode {
	ControlFlowNodeKind kind;
	b8 definitely_returns;
	b8 unreachable;
};

struct ControlFlowNodeUnconditional : ControlFlowNode {
	Array<SyntaxNode*> syntax_nodes;
};

struct ControlFlowNodeIfElse : ControlFlowNode {
	Array<SyntaxNode*> condition_syntax_nodes;
	
	Array<ControlFlowNode*> if_control_flow_nodes;
	Array<ControlFlowNode*> else_control_flow_nodes;
	
	SyntaxNodeIfElse* syntax_node;
};

template<typename ControlFlowNodeType>
internal ControlFlowNodeType* create_control_flow_node_internal(ControlFlowNodeKind kind) {
	auto new_control_flow_node = allocate<ControlFlowNodeType>(&heap_stack);
	new_control_flow_node->kind = kind;
	return new_control_flow_node;
}
#define create_control_flow_node(ControlFlowNodeType) create_control_flow_node_internal<ControlFlowNode##ControlFlowNodeType>(ControlFlowNodeKind::##ControlFlowNodeType)

//---Parser
enum class SyntaxNodeKind : u8 {
	Invalid,
	BinaryMathOperation,
	Assignment,
	Return,
	Identifier,
	Declaration,
	ArrayAccess,
	Function,
	IfElse,
	IntegerLiteral,
	StringLiteral,
	AtomicType,
	FixedLengthArrayType,
};

struct SyntaxNode {
	SourceSite* site;
	SyntaxNodeKind kind;
	union {
		Value* result;
		Type* type_result;
	};
};

struct SyntaxNodeBinaryMathOperation : SyntaxNode {
	Keyword operator_keyword;
	SyntaxNode* operands[2];
};

struct SyntaxNodeAssignment : SyntaxNode {
	SyntaxNode* destination_expression;
	SyntaxNode* source_expression;
};

struct SyntaxNodeReturn : SyntaxNode {
	SyntaxNode* return_expression;
};

struct SyntaxNodeIdentifier : SyntaxNode {
	ConstString identifier;
};

struct SyntaxNodeDeclaration : SyntaxNode {
	//@rename identifier node
	SyntaxNodeIdentifier* identifier;
	SyntaxNode* type_node;
};

struct SyntaxNodeArrayAccess : SyntaxNode {
	SyntaxNode* array_expression;
	SyntaxNode* index_expression;
};

struct SyntaxNodeFunction : SyntaxNode {
	struct Argument {
		SourceSite* site; //@rename declaration_site
		ConstString identifier;
		SyntaxNode* type_node;
		Value* value;
	};
	
	ConstString identifier;
	Array<Argument> arguments;
	Array<SyntaxNode*> return_type_nodes;
	Array<Type*> return_types;
	
	ScopedStatements body;
	Array<ControlFlowNode*> control_flow_nodes;
};

struct SyntaxNodeIfElse : SyntaxNode {
	SyntaxNode* condition_expression;
	ScopedStatements if_body;
	ScopedStatements else_body;
};

struct SyntaxNodeIntegerLiteral : SyntaxNode {
	u64 integer;
};

struct SyntaxNodeStringLiteral : SyntaxNode {
	ConstString string;
};

struct SyntaxNodeAtomicType : SyntaxNode {
	TypeAtomic atomic_type;
};

struct SyntaxNodeFixedLengthArrayType: SyntaxNode {
	u64 length;
	SyntaxNode* element_type_node;
};

struct ParseContext {
	Token* cursor;
	Token* tokens_end;
};

template<typename AllocatorType> internal void write(String* buffer, Array<SyntaxNode*> syntax_nodes, fuint indent, AllocatorType* allocator);
template<typename AllocatorType> internal void write_function_opening(String* buffer, SyntaxNodeFunction* function, AllocatorType* allocator);

template<typename SyntaxNodeType>
internal SyntaxNodeType* create_syntax_node_internal(SyntaxNodeKind kind, SourceSite* site) {
	auto syntax_node = allocate<SyntaxNodeType>(&heap_stack);
	syntax_node->kind = kind;
	syntax_node->site = site;
	return syntax_node;
}
#define create_syntax_node(type, site) create_syntax_node_internal<SyntaxNode##type>(SyntaxNodeKind::##type, site)

template<typename AllocatorType>
internal void write(String* buffer, SyntaxNode* syntax_node, fuint indent, AllocatorType* allocator) {
	switch (syntax_node->kind) {
		case SyntaxNodeKind::BinaryMathOperation: {
			auto binary_math_operation = (SyntaxNodeBinaryMathOperation*)syntax_node;
			write(buffer, "(", allocator);
			write(buffer, binary_math_operation->operands[0], indent, allocator);
			write(buffer, " ", allocator);
			write(buffer, binary_math_operation->operator_keyword, allocator);
			write(buffer, " ", allocator);
			write(buffer, binary_math_operation->operands[1], indent, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case SyntaxNodeKind::Assignment: {
			auto assignment = (SyntaxNodeAssignment*)syntax_node;
			write(buffer, "(", allocator);
			write(buffer, assignment->destination_expression, indent, allocator);
			write(buffer, " = ", allocator);
			write(buffer, assignment->source_expression, indent, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case SyntaxNodeKind::Return: {
			auto return_node = (SyntaxNodeReturn*)syntax_node;
			write(buffer, "return ", allocator);
			write(buffer, return_node->return_expression, indent, allocator);
		} break;
		
		case SyntaxNodeKind::Identifier: {
			auto identifier = (SyntaxNodeIdentifier*)syntax_node;
			write(buffer, identifier->identifier, allocator);
		} break;
		
		case SyntaxNodeKind::Declaration: {
			auto declaration = (SyntaxNodeDeclaration*)syntax_node;
			write(buffer, declaration->identifier->identifier, allocator);
			write(buffer, ": ", allocator);
			write(buffer, declaration->type_node, indent, allocator);
		} break;
		
		case SyntaxNodeKind::ArrayAccess: {
			auto array_access = (SyntaxNodeArrayAccess*)syntax_node;
			write(buffer, "(", allocator);
			write(buffer, array_access->array_expression, indent, allocator);
			write(buffer, "[", allocator);
			write(buffer, array_access->index_expression, indent, allocator);
			write(buffer, "]", allocator);
			write(buffer, ")", allocator);
		} break;
		
		case SyntaxNodeKind::Function: {
			auto function = (SyntaxNodeFunction*)syntax_node;
			write_function_opening(buffer, function, allocator);
			write(buffer, function->body.statements, indent + 1, allocator);
			write_indent(buffer, indent, allocator);
			write(buffer, "}", allocator);
		} break;
		
		case SyntaxNodeKind::IfElse: {
			auto if_else = (SyntaxNodeIfElse*)syntax_node;
			write(buffer, "if ", allocator);
			write(buffer, if_else->condition_expression, indent, allocator);
			write(buffer, " {\n", allocator);
			write(buffer, if_else->if_body.statements, indent + 1, allocator);
			write_indent(buffer, indent, allocator);
			write(buffer, "}", allocator);
			if (if_else->else_body.statements) {
				write(buffer, " else {\n", allocator);
				write(buffer, if_else->else_body.statements, indent + 1, allocator);
				write_indent(buffer, indent, allocator);
				write(buffer, "}\n", allocator);
			} else {
				write(buffer, "\n", allocator);
			}
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
		
		case SyntaxNodeKind::FixedLengthArrayType: {
			auto array_type_node = (SyntaxNodeFixedLengthArrayType*)syntax_node;
			write(buffer, "[", allocator);
			write_uint(buffer, array_type_node->length, allocator);
			write(buffer, "]", allocator);
			write(buffer, array_type_node->element_type_node, indent, allocator);
		} break;
		
		case SyntaxNodeKind::AtomicType: {
			auto atomic_type_node = (SyntaxNodeAtomicType*)syntax_node;
			//@todo pull out writer function for TypeAtomic
			write(buffer, (Type*)&atomic_type_node->atomic_type, allocator);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

template<typename AllocatorType>
internal void write(String* buffer, Array<SyntaxNode*> syntax_nodes, fuint indent, AllocatorType* allocator) {
	fox_for (syntax_node_index, syntax_nodes.length) {
		write_indent(buffer, indent, allocator);
		write(buffer, syntax_nodes[syntax_node_index], indent, allocator);
		write(buffer, "\n", allocator);
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
		write(buffer, argument->type_node, 0, allocator);
		if (argument_index != function->arguments.length - 1) {
			write(buffer, ", ", allocator);
		}
	}
	write(buffer, ")", allocator);
	
	if (function->return_type_nodes) {
		write(buffer, ": ", allocator);
		fox_for (return_index, function->return_type_nodes.length) {
			write(buffer, function->return_type_nodes[return_index], 0, allocator);
			if (return_index != function->return_type_nodes - 1) {
				write(buffer, ", ", allocator);
			}
		}
	}
	
	write(buffer, " {\n", allocator);
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
#define expect_token_kind(kind, where) if (!expect_token_kind_internal(parser, (kind), (where))) { return nullptr; }

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
#define expect_keyword(keyword, where) if (!expect_keyword_internal(parser, (keyword), (where))) { return nullptr; }

internal bool expect_inside_function_internal(ParseContext* parser, bool inside_function, ConstString expect_what) {
	if (!inside_function) {
		String error_message;
		zero(&error_message);
		write(&error_message, parser->cursor->site, &heap_stack);
		write(&error_message, "Syntax error: ", &heap_stack);
		write(&error_message, expect_what, &heap_stack);
		write(&error_message, " is only allowed inside a function.\n", &heap_stack);
		print(error_message);
	}
	return inside_function;
}
#define expect_inside_function(where) if (!expect_inside_function_internal(parser, inside_function, (where))) { return nil; }

internal Optional<Array<SyntaxNode*>> parse_statements(ParseContext* parser, bool inside_function = true);

internal SyntaxNode* parse_type(ParseContext* parser) {
	//@todo better error message
	expect_token_kind(TokenKind::keyword, "in type-y place, expected type.\n");
	
	if (is_integer_type_keyword(parser->cursor->keyword_value)) {
		auto integer_keyword = parser->cursor->keyword_value;
		auto is_signed = is_signed_integer_type_keyword(integer_keyword);
		auto size = get_size_of_integer_type_keyword(integer_keyword, is_signed);
		
		auto atomic_type_node = create_syntax_node(AtomicType, &parser->cursor->site);
		atomic_type_node->atomic_type.kind = TypeKind::Atomic;
		atomic_type_node->atomic_type.is_signed = is_signed;
		atomic_type_node->atomic_type.size = size;
		
		parser->cursor++;
		return atomic_type_node;
	} else if (parser->cursor->keyword_value == Keyword::open_square) {
		auto array_type_site = &parser->cursor->site;
		parser->cursor++;
		
		expect_token_kind(TokenKind::integer, "in type declaration, expected integer literal length of array.\n");
		u64 array_length = parser->cursor->integer_value;
		parser->cursor++;
		expect_keyword(Keyword::close_square, "in type declaration, expected close ].\n");
		auto element_type_node = parse_type(parser);
		
		auto array_type_node = create_syntax_node(FixedLengthArrayType, array_type_site);
		array_type_node->length = array_length;
		array_type_node->element_type_node = element_type_node;
		return array_type_node;
	}
	
	String error_message;
	zero(&error_message);
	write(&error_message, parser->cursor->site, &heap_stack);
	write(&error_message, "Syntax error: expected type.\n", &heap_stack);
	print(error_message);
	return nullptr;
}

internal SyntaxNode* parse_expression(ParseContext* parser, u64 outer_precedence = 0) {
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
		
		//@todo parenthesis
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
	
	//parse declarations
	if (parser->cursor < parser->tokens_end
		&& parser->cursor->kind == TokenKind::keyword
		&& parser->cursor->keyword_value == Keyword::declare) {
		if (left->kind != SyntaxNodeKind::Identifier) {
			String error_message;
			zero(&error_message);
			write(&error_message, *left->site, &heap_stack);
			write(&error_message, "Syntax Error: expected identifier in declaration.\n", &heap_stack);
			print(error_message);
			return nullptr;
		}
		
		auto declaration_site = &parser->cursor->site;
		parser->cursor++;
		auto declaration_type_node = parse_type(parser);
		auto declaration_node = create_syntax_node(Declaration, declaration_site);
		declaration_node->identifier = (SyntaxNodeIdentifier*)left;
		declaration_node->type_node = declaration_type_node;
		left = declaration_node;
	}
	
	//parse array access expressions
	while (parser->cursor < parser->tokens_end
		   && parser->cursor->kind == TokenKind::keyword
		   && parser->cursor->keyword_value == Keyword::open_square) {
		auto array_access_site = &parser->cursor->site;
		parser->cursor++;
		auto index_expression = parse_expression(parser);
		expect_keyword(Keyword::close_square, "in array access, expected close square ].\n");
		
		auto array_access = create_syntax_node(ArrayAccess, array_access_site);
		array_access->array_expression = left;
		array_access->index_expression = index_expression;
		
		left = array_access;
	}
	
	//parse binary operators if precedence dictates
	while (parser->cursor < parser->tokens_end
		   && parser->cursor->kind == TokenKind::keyword
		   && is_binary_operator(parser->cursor->keyword_value)) {
		auto operator_keyword = parser->cursor->keyword_value;
		auto precedence = precedences[(fuint)operator_keyword];
		if (precedence > outer_precedence || (precedence == outer_precedence && is_right_associative(operator_keyword))) {
			auto binary_operation_site = &parser->cursor->site;
			auto binary_operator = parser->cursor->keyword_value;
			parser->cursor++;
			
			if (parser->cursor < parser->tokens_end) {
				SyntaxNode* right = nullptr;
				right = parse_expression(parser, precedence);
				
				if (right) {
					if (binary_operator == Keyword::assign) {
						auto assignment = create_syntax_node(Assignment, binary_operation_site);
						assignment->destination_expression = left;
						assignment->source_expression = right;
						
						left = assignment;
					} else {
						auto binary_math_operation = create_syntax_node(BinaryMathOperation, binary_operation_site);
						binary_math_operation->operator_keyword = binary_operator;
						binary_math_operation->operands[0] = left;
						binary_math_operation->operands[1] = right;
						
						left = binary_math_operation;
					}
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

internal SyntaxNodeIfElse* parse_if_else(ParseContext* parser) {
	fox_assert(parser->cursor < parser->tokens_end
			   && parser->cursor->kind == TokenKind::keyword
			   && parser->cursor->keyword_value == Keyword::if_keyword);
	auto if_site = &parser->cursor->site;
	parser->cursor++;
	
	auto condition_expression = parse_expression(parser);
	expect_keyword(Keyword::open_curly, "in if statement, expected if body.\n");
	auto if_body = parse_statements(parser, true);
	if (!if_body) {
		return nullptr;
	}
	expect_keyword(Keyword::close_curly, "in if statement body.\n");
	
	//Check for else
	Array<SyntaxNode*> else_body;
	zero(&else_body);
	if (parser->cursor < parser->tokens_end
		&& parser->cursor->kind == TokenKind::keyword
		&& parser->cursor->keyword_value == Keyword::else_keyword) {
		parser->cursor++;
		if (parser->cursor >= parser->tokens_end) {
			print_unexpected_end_of_file(parser->cursor[-1].site, "in else statement body.\n");
		}
		
		if (parser->cursor->kind == TokenKind::keyword
			&& parser->cursor->keyword_value == Keyword::if_keyword) {
			auto if_else_for_else = parse_if_else(parser);
			if (if_else_for_else) {
				push(&else_body, if_else_for_else, &heap_stack);
			} else {
				return nullptr;
			}
		} else {
			expect_keyword(Keyword::open_curly, "in else statement body.\n");
			auto maybe_else_body = parse_statements(parser, true);
			if (!maybe_else_body) {
				return nullptr;
			}
			else_body = maybe_else_body.value;
			expect_keyword(Keyword::close_curly, "in else statement body.\n");
		}
	}
	
	auto if_else = create_syntax_node(IfElse, if_site);
	if_else->condition_expression = condition_expression;
	if_else->if_body.statements = if_body.value;
	if_else->else_body.statements = else_body;
	return if_else;
}

internal SyntaxNodeFunction* parse_function(ParseContext* parser) {
	//function name(argument1: int, argument2: int,): int {}
	//function name() {}
	fox_assert(parser->cursor->kind == TokenKind::keyword && parser->cursor->keyword_value == Keyword::function);
	auto function_site = &parser->cursor->site;
	parser->cursor++;
	
	expect_token_kind(TokenKind::identifier, "in function declaration, expected function name.\n");
	auto function_identifier = parser->cursor->identifier_value;
	parser->cursor++;
	
	expect_keyword(Keyword::open_parenthesis, "in function declaration, expected open parenthesis.\n");
	
	//Parse arguments
	Array<SyntaxNodeFunction::Argument> arguments;
	zero(&arguments);
	while (true) {
		if (parser->cursor >= parser->tokens_end) {
			print_unexpected_end_of_file(parser->cursor[-1].site, "in function declaration, expected close parenthesis or next argument.\n");
			return nullptr;
		}
		
		if (parser->cursor->kind == TokenKind::keyword) {
			expect_keyword(Keyword::close_parenthesis, "in function declaration, expected close parenthesis or next argument.\n");
			break;
		}
		
		expect_token_kind(TokenKind::identifier, "in function declaration, expected name of argument.\n");
		auto argument_identifier = parser->cursor->identifier_value;
		parser->cursor++;
		auto argument_site = &parser->cursor->site;
		expect_keyword(Keyword::declare, "in function declaration, expected type of argumnet.\n");
		auto argument_type = parse_type(parser);
		
		auto new_argument = push_zero(&arguments, &heap_stack);
		new_argument->site = argument_site;
		new_argument->identifier = argument_identifier;
		new_argument->type_node = argument_type;
		
		expect_token_kind(TokenKind::keyword, "in function declaration, expected close parenthesis or next argument.\n");
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
	
	expect_token_kind(TokenKind::keyword, "in function declaration, expected return value or function body.\n");
	
	Array<SyntaxNode*> return_type_nodes;
	zero(&return_type_nodes);
	if (parser->cursor->keyword_value == Keyword::declare) {
		parser->cursor++;
		
		//@todo we should probably support terminating comma here too
		while (true) {
			auto return_type = parse_type(parser);
			push(&return_type_nodes, return_type, &heap_stack);
			
			expect_token_kind(TokenKind::keyword, "in function declaration, expected next return value or function body.\n");
			if (parser->cursor->keyword_value == Keyword::open_curly) {
				break;
			}
			expect_keyword(Keyword::comma, "in function declaration, expected next return value or function body.\n");
			parser->cursor++;
		}
	}
	
	//@todo clean this up a bit, function x() , int {} will tell us we expected function body at comma
	expect_keyword(Keyword::open_curly, "in function declaration, expected function body.\n");
	auto function_body = parse_statements(parser);
	if (function_body) {
		expect_keyword(Keyword::close_curly, "in function declaration, expected end of function body.\n");
		
		auto function = create_syntax_node(Function, function_site);
		function->identifier = function_identifier;
		function->arguments = arguments;
		function->return_type_nodes = return_type_nodes;
		function->body.statements = function_body.value;
		return function;
	}
	
	return nullptr;
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
				
				case Keyword::if_keyword: {
					expect_inside_function("if statement");
					auto if_else = parse_if_else(parser);
					if (if_else) {
						push(&statements, if_else, &heap_stack);
					} else {
						return nil;
					}
				} break;
				
				case Keyword::return_keyword: {
					expect_inside_function("return");
					auto return_site = &parser->cursor->site;
					parser->cursor++;
					auto return_expression = parse_expression(parser);
					if (return_expression) {
						auto return_node = create_syntax_node(Return, return_site);
						return_node->return_expression = return_expression;
						push(&statements, return_node, &heap_stack);
					} else {
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
				
				//@todo specific message for else
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

internal Optional<Array<SyntaxNode*>> parse(Array<Token> tokens) {
	fox_assert(tokens);
	
	ParseContext parser;
	zero(&parser);
	parser.cursor = tokens.data;
	parser.tokens_end = tokens.data + tokens.length;
	
	return parse_statements(&parser, false);
}

//---Linearizer
internal bool linearize_function(LinearizerContext* linearizer, SyntaxNodeFunction* function);
internal bool linearize_scoped_statements(LinearizerContext* linearizer, ScopedStatements* scoped_statements, Array<ControlFlowNode*>* control_flow_nodes_output);
internal bool linearize_inner_scoped_statements(LinearizerContext* linearizer, ScopedStatements* scoped_statements, ScopedStatements* outer_scope, Array<ControlFlowNode*>* control_flow_nodes_output);

internal void print_statement_does_nothing_warning(SyntaxNode* statement) {
	auto restore_point = create_restore_point(&heap_stack);
	String error_message;
	zero(&error_message);
	write(&error_message, *statement->site, &heap_stack);
	write(&error_message, "Warning: statement does nothing.\n", &heap_stack);
	print(error_message);
	restore(&heap_stack, restore_point);
}

internal void linearize_type(LinearizerContext* linearizer, LinearizedType* linearized_type, SyntaxNode* type_node) {
	switch (type_node->kind) {
		case SyntaxNodeKind::AtomicType: {
			push(&linearized_type->type_nodes, type_node, &heap_stack);
		} break;
		
		case SyntaxNodeKind::FixedLengthArrayType: {
			auto array_type = (SyntaxNodeFixedLengthArrayType*)type_node;
			linearize_type(linearizer, linearized_type, array_type->element_type_node);
			push(&linearized_type->type_nodes, type_node, &heap_stack);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

//@rename result_consumer? or linearized_type->result?
internal void create_linearized_type(LinearizerContext* linearizer, SyntaxNode* type_node, Type** result_consumer) {
	auto linearized_type = allocate<LinearizedType>(&heap_stack);
	linearized_type->result = result_consumer;
	linearize_type(linearizer, linearized_type, type_node);
	push(&linearizer->linearized_types, linearized_type, &heap_stack);
}

#define push_dependency_order_syntax_node(syntax_node) push(dependency_order_syntax_nodes, (syntax_node), &heap_stack)

internal bool linearize_syntax_node(LinearizerContext* linearizer, SyntaxNode* syntax_node, ScopedStatements* scope, Array<SyntaxNode*>* dependency_order_syntax_nodes, bool is_statement = false) {
	switch (syntax_node->kind) {
		case SyntaxNodeKind::BinaryMathOperation: {
			auto binary_math_operation = (SyntaxNodeBinaryMathOperation*)syntax_node;
			if (!(linearize_syntax_node(linearizer, binary_math_operation->operands[0], scope, dependency_order_syntax_nodes)
				  && linearize_syntax_node(linearizer, binary_math_operation->operands[1], scope, dependency_order_syntax_nodes))) {
				return false;
			}
			
			push(dependency_order_syntax_nodes, binary_math_operation, &heap_stack);
		} break;
		
		case SyntaxNodeKind::Assignment: {
			auto assignment = (SyntaxNodeAssignment*)syntax_node;
			if (!(linearize_syntax_node(linearizer, assignment->destination_expression, scope, dependency_order_syntax_nodes)
				  && linearize_syntax_node(linearizer, assignment->source_expression, scope, dependency_order_syntax_nodes))) {
				return false;
			}
			
			push(dependency_order_syntax_nodes, assignment, &heap_stack);
		} break;
		
		case SyntaxNodeKind::Declaration: {
			auto declaration = (SyntaxNodeDeclaration*)syntax_node;
			auto identifier = declaration->identifier->identifier;
			
			fox_for (value_index, scope->declarations.length) {
				auto value = scope->declarations[value_index];
				if (value->identifier == identifier) {
					String error_message;
					zero(&error_message);
					write(&error_message, *declaration->site, &heap_stack);
					write(&error_message, "Syntax Error: redeclaration of previously declared value.\n", &heap_stack);
					write(&error_message, *value->declaration_site, &heap_stack);
					write(&error_message, "Note: previous declaration is here.\n", &heap_stack);
					print(error_message);
					return false;
				}
			}
			
			auto new_value = allocate<Value>(&heap_stack);
			new_value->identifier = identifier;
			create_linearized_type(linearizer, declaration->type_node, &new_value->type);
			push(&scope->declarations, new_value, &heap_stack);
			declaration->result = new_value;
			
			push(dependency_order_syntax_nodes, declaration, &heap_stack);
		} break;
		
		case SyntaxNodeKind::ArrayAccess: {
			auto array_access = (SyntaxNodeArrayAccess*)syntax_node;
			if (!(linearize_syntax_node(linearizer, array_access->array_expression, scope, dependency_order_syntax_nodes)
				  && linearize_syntax_node(linearizer, array_access->index_expression, scope, dependency_order_syntax_nodes))) {
				return false;
			}
			
			push(dependency_order_syntax_nodes, array_access, &heap_stack);
		} break;
		
		case SyntaxNodeKind::Identifier:
		case SyntaxNodeKind::IntegerLiteral:
		case SyntaxNodeKind::StringLiteral: {
			if (is_statement) {
				print_statement_does_nothing_warning(syntax_node);
			}
			
			push(dependency_order_syntax_nodes, syntax_node, &heap_stack);
		} break;
		
		case SyntaxNodeKind::Function: {
			auto function_node = (SyntaxNodeFunction*)syntax_node;
			if (!linearize_function(linearizer, function_node)) {
				return false;
			}
		} break;
		
		case SyntaxNodeKind::Return: //Handled in linearize_scoped_statements
		case SyntaxNodeKind::IfElse: //Handled in linearize_scoped_statements
		case SyntaxNodeKind::AtomicType:
		default: {
			fox_unreachable;
		} break;
	}
	
	return true;
}

internal bool linearize_scoped_statements(LinearizerContext* linearizer, ScopedStatements* scoped_statements, Array<ControlFlowNode*>* control_flow_nodes_output) {
	ControlFlowNode* current_control_flow_node = nullptr;
	Array<SyntaxNode*>* dependency_order_syntax_nodes = nullptr;
	
	auto terminate_control_flow_node = [&]() {
		push(control_flow_nodes_output, current_control_flow_node, &heap_stack);
		current_control_flow_node = nullptr;
		dependency_order_syntax_nodes = nullptr;
	};
	
	fox_for (statement_index, scoped_statements->statements.length) {
		auto statement = scoped_statements->statements[statement_index];
		
		switch (statement->kind) {
			case SyntaxNodeKind::IfElse: {
				if (current_control_flow_node) {
					terminate_control_flow_node();
				}
				
				auto if_else_control_flow = create_control_flow_node(IfElse);
				auto if_else_syntax_node = (SyntaxNodeIfElse*)statement;
				if_else_control_flow->syntax_node = if_else_syntax_node;
				
				if (!(linearize_syntax_node(linearizer, if_else_syntax_node->condition_expression, scoped_statements, &if_else_control_flow->condition_syntax_nodes)
					  && linearize_inner_scoped_statements(linearizer, &if_else_syntax_node->if_body, scoped_statements, &if_else_control_flow->if_control_flow_nodes)
					  && linearize_inner_scoped_statements(linearizer, &if_else_syntax_node->else_body, scoped_statements, &if_else_control_flow->else_control_flow_nodes))) {
					return false;
				}
				
				push(control_flow_nodes_output, if_else_control_flow, &heap_stack);
			} break;
			
			case SyntaxNodeKind::Return: {
				if (current_control_flow_node == nullptr) {
					auto unconditional_control_flow_node = create_control_flow_node(Unconditional);
					current_control_flow_node = unconditional_control_flow_node;
					dependency_order_syntax_nodes = &unconditional_control_flow_node->syntax_nodes;
				}
				
				auto return_node = (SyntaxNodeReturn*)statement;
				if (!linearize_syntax_node(linearizer, return_node->return_expression, scoped_statements, dependency_order_syntax_nodes)) {
					return false;
				}
				
				push(dependency_order_syntax_nodes, return_node, &heap_stack);
				
				current_control_flow_node->definitely_returns = true;
				terminate_control_flow_node();
			} break;
			
			default: {
				if (current_control_flow_node == nullptr) {
					auto unconditional_control_flow_node = create_control_flow_node(Unconditional);
					current_control_flow_node = unconditional_control_flow_node;
					dependency_order_syntax_nodes = &unconditional_control_flow_node->syntax_nodes;
				}
				
				if (!linearize_syntax_node(linearizer, statement, scoped_statements, dependency_order_syntax_nodes)) {
					return false;
				}
			} break;
		}
	}
	
	if (current_control_flow_node) {
		push(control_flow_nodes_output, current_control_flow_node, &heap_stack);
	}
	
	return true;
}

internal bool linearize_inner_scoped_statements(LinearizerContext* linearizer, ScopedStatements* scoped_statements, ScopedStatements* outer_scope, Array<ControlFlowNode*>* control_flow_nodes_output) {
	scoped_statements->containing_function = outer_scope->containing_function;
	scoped_statements->outer_scope = outer_scope;
	
	return linearize_scoped_statements(linearizer, scoped_statements, control_flow_nodes_output);
}

internal bool linearize_function(LinearizerContext* linearizer, SyntaxNodeFunction* function) {
	push(&linearizer->functions, function, &heap_stack);
	
	function->body.containing_function = function;
	
	fox_for (argument_index, function->arguments.length) {
		auto argument = &function->arguments[argument_index];
		auto argument_value = allocate<Value>(&heap_stack);
		argument_value->identifier = argument->identifier;
		create_linearized_type(linearizer, argument->type_node, &argument_value->type);
		argument_value->declaration_site = argument->site;
		argument_value->is_argument = true;
		argument->value = argument_value;
		push(&function->body.declarations, argument_value, &heap_stack);
	}
	
	function->return_types = allocate_array<Type*>(function->return_type_nodes, &heap_stack);
	fox_for (return_type_index, function->return_types.length) {
		create_linearized_type(linearizer, function->return_type_nodes[return_type_index], &function->return_types[return_type_index]);
	}
	
	return linearize_scoped_statements(linearizer, &function->body, &function->control_flow_nodes);
}

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

template<typename AllocatorType>
internal void write(String* buffer, ControlFlowNode* control_flow_node, fuint indent, AllocatorType* allocator);

template<typename AllocatorType>
internal void write(String* buffer, Array<ControlFlowNode*> control_flow_nodes, fuint indent, AllocatorType* allocator) {
	fox_for (control_flow_node_index, control_flow_nodes.length) {
		write(buffer, control_flow_nodes[control_flow_node_index], indent, allocator);
	}
}

template<typename ArrayType, typename AllocatorType>
internal void write_control_flow_block(String* buffer, ConstString block_name, ArrayType control_flow_nodes_or_syntax_nodes, fuint indent, AllocatorType* allocator) {
	write_indent(buffer, indent, allocator);
	write(buffer, block_name, allocator);
	write(buffer, " {\n", allocator);
	write(buffer, control_flow_nodes_or_syntax_nodes, indent + 1, allocator);
	write_indent(buffer, indent, allocator);
	write(buffer, "}\n", allocator);
}

template<typename AllocatorType>
internal void write(String* buffer, ControlFlowNode* control_flow_node, fuint indent, AllocatorType* allocator) {
	switch (control_flow_node->kind) {
		case ControlFlowNodeKind::Unconditional: {
			auto unconditional_control_flow_node = (ControlFlowNodeUnconditional*)control_flow_node;
			write_control_flow_block(buffer, "Unconditional", unconditional_control_flow_node->syntax_nodes, indent, allocator);
		} break;
		
		case ControlFlowNodeKind::IfElse: {
			auto if_else_control_flow_node = (ControlFlowNodeIfElse*)control_flow_node;
			write_control_flow_block(buffer, "IfElse Condition", if_else_control_flow_node->condition_syntax_nodes, indent, allocator);
			write_control_flow_block(buffer, "IfElse If", if_else_control_flow_node->if_control_flow_nodes, indent, allocator);
			write_control_flow_block(buffer, "IfElse Else", if_else_control_flow_node->else_control_flow_nodes, indent, allocator);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

template<typename AllocatorType>
internal void write(String* buffer, LinearizerContext* linearizer, AllocatorType* allocator) {
	fox_for (function_index, linearizer->functions.length) {
		auto function = linearizer->functions[function_index];
		write_function_opening(buffer, function, allocator);
		fox_for (control_flow_node_index, function->control_flow_nodes.length) {
			write(buffer, function->control_flow_nodes[control_flow_node_index], 1, allocator);
		}
		write(buffer, "}\n\n", allocator);
	}
}

//---Validater
internal bool validate_syntax_node_semantics(SyntaxNode* syntax_node, ScopedStatements* scope) {
	switch (syntax_node->kind) {
		case SyntaxNodeKind::BinaryMathOperation: {
			auto binary_math_operation = (SyntaxNodeBinaryMathOperation*)syntax_node;
			auto operands = binary_math_operation->operands;
			
			fox_for (operand_index, 2) {
				auto operand = binary_math_operation->operands[operand_index];
				if (operand->result->type->kind != TypeKind::Atomic) {
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
			
			//@todo automatic up-casting
			if (*operands[0]->result->type != *operands[1]->result->type) {
				String error_message;
				zero(&error_message);
				write(&error_message, *binary_math_operation->site, &heap_stack);
				write(&error_message, "Type error: operands must be same type.\n", &heap_stack);
				write(&error_message, "Types are ", &heap_stack);
				write(&error_message, operands[0]->result->type, &heap_stack);
				write(&error_message, " and ", &heap_stack);
				write(&error_message, operands[1]->result->type, &heap_stack);
				write(&error_message, "\n", &heap_stack);
				print(error_message);
				return false;
			}
			
			syntax_node->result = allocate<Value>(&heap_stack);
			syntax_node->result->type = operands[0]->result->type;
		} break;
		
		case SyntaxNodeKind::Assignment: {
			//@bug we are not checking if the left side is an l-value
			auto assignment = (SyntaxNodeAssignment*)syntax_node;
			auto destination_expression = assignment->destination_expression;
			auto source_expression = assignment->source_expression;
			if (*destination_expression->result->type != *source_expression->result->type) {
				String error_message;
				zero(&error_message);
				write(&error_message, *assignment->site, &heap_stack);
				write(&error_message, "Type error: cannot assign ", &heap_stack);
				write(&error_message, source_expression->result->type, &heap_stack);
				write(&error_message, " to ", &heap_stack);
				write(&error_message, destination_expression->result->type, &heap_stack);
				write(&error_message, "\n", &heap_stack);
				print(error_message);
				return false;
			}
			
			assignment->result = destination_expression->result;
		} break;
		
		case SyntaxNodeKind::Return: {
			auto return_node = (SyntaxNodeReturn*)syntax_node;
			auto containing_function = scope->containing_function;
			fox_assert(containing_function->return_types.length == 1); //@bug support multiple returns
			if (*return_node->return_expression->result->type != *containing_function->return_types[0]) {
				String error_message;
				zero(&error_message);
				write(&error_message, *return_node->return_expression->site, &heap_stack);
				write(&error_message, "Type error: expected return expression to be type ", &heap_stack);
				write(&error_message, containing_function->return_types[0], &heap_stack);
				write(&error_message, " but got ", &heap_stack);
				write(&error_message, return_node->return_expression->result->type, &heap_stack);
				write(&error_message, "\n", &heap_stack);
				print(error_message);
				return false;
			}
		} break;
		
		case SyntaxNodeKind::Identifier: {
			auto identifier = ((SyntaxNodeIdentifier*)syntax_node)->identifier;
			
			ScopedStatements* current_scope = scope;
			while (current_scope) {
				fox_for (declaration_index, current_scope->declarations.length) {
					auto declaration = current_scope->declarations[declaration_index];
					if (declaration->identifier == identifier) {
						syntax_node->result = declaration;
						return true;
					}
				}
				
				current_scope = current_scope->outer_scope;
			}
			
			String error_message;
			zero(&error_message);
			write(&error_message, *syntax_node->site, &heap_stack);
			write(&error_message, "Semantic error: undeclared identifier ", &heap_stack);
			write(&error_message, identifier, &heap_stack);
			write(&error_message, "\n", &heap_stack);
			print(error_message);
			return false;
		} break;
		
		case SyntaxNodeKind::Declaration: {
			//value creation is handled in linearization
			//typing is handled during type generation
			//do nothing
		} break;
		
		case SyntaxNodeKind::ArrayAccess: {
			auto array_access = (SyntaxNodeArrayAccess*)syntax_node;
			
			auto array_expression = array_access->array_expression;
			auto array_type = array_expression->result->type;
			if (array_type->kind != TypeKind::FixedLengthArray) {
				String error_message;
				zero(&error_message);
				write(&error_message, *array_expression->site, &heap_stack);
				write(&error_message, "Semantic error: expected array on left hand side of array access but got ", &heap_stack);
				write(&error_message, array_type, &heap_stack);
				write(&error_message, "\n", &heap_stack);
				print(error_message);
				return false;
			}
			
			auto index_expression = array_access->index_expression;
			auto index_type = index_expression->result->type;
			if (index_type->kind != TypeKind::Atomic) {
				String error_message;
				zero(&error_message);
				write(&error_message, *index_expression->site, &heap_stack);
				write(&error_message, "Semantic error: expected integer index in array access but got ", &heap_stack);
				write(&error_message, index_type, &heap_stack);
				write(&error_message, "\n", &heap_stack);
				print(error_message);
				return false;
			}
			
			auto element_value = allocate<Value>(&heap_stack);
			element_value->type = ((TypeFixedLengthArray*)array_type)->element_type;
			array_access->result = element_value;
		} break;
		
		case SyntaxNodeKind::IntegerLiteral: {
			auto integer_type = create_type(Atomic);
			integer_type->is_signed = true;
			integer_type->size = 8;
			
			auto integer_value = allocate<Value>(&heap_stack);
			integer_value->type = integer_type;
			
			syntax_node->result = integer_value;
		} break;
		
		case SyntaxNodeKind::StringLiteral: {
			auto string_node = (SyntaxNodeStringLiteral*)syntax_node;
			
			auto u8_type = create_type(Atomic);
			u8_type->size = 1;
			
			auto string_type = create_type(FixedLengthArray);
			string_type->length = string_node->string.length;
			string_type->element_type = u8_type;
			
			auto string_value = allocate<Value>(&heap_stack);
			string_value->type = string_type;
			
			string_node->result = string_value;
		} break;
		
		case SyntaxNodeKind::Function:
		case SyntaxNodeKind::IfElse: //Handled in validate_scoped_statements_semantics
		case SyntaxNodeKind::AtomicType:
		case SyntaxNodeKind::FixedLengthArrayType:
		default: {
			fox_unreachable;
		}
	}
	
	return true;
}

internal bool validate_control_flow_nodes(Array<ControlFlowNode*> control_flow_nodes, ScopedStatements* scope) {
	fox_for (control_flow_node_index, control_flow_nodes.length) {
		auto control_flow_node = control_flow_nodes[control_flow_node_index];
		switch (control_flow_node->kind) {
			case ControlFlowNodeKind::Unconditional: {
				auto uncondition_control_flow_node = (ControlFlowNodeUnconditional*)control_flow_node;
				fox_for (syntax_node_index, uncondition_control_flow_node->syntax_nodes.length) {
					if (!validate_syntax_node_semantics(uncondition_control_flow_node->syntax_nodes[syntax_node_index], scope)) {
						return false;
					}
				}
			} break;
			
			case ControlFlowNodeKind::IfElse: {
				auto if_else_control_flow_node = (ControlFlowNodeIfElse*)control_flow_node;
				
				//Validate the condition syntax nodes
				fox_for (syntax_node_index, if_else_control_flow_node->condition_syntax_nodes.length) {
					if (!validate_syntax_node_semantics(if_else_control_flow_node->condition_syntax_nodes[syntax_node_index], scope)) {
						return false;
					}
				}
				
				//Validate the node itself
				auto if_else_syntax_node = if_else_control_flow_node->syntax_node;
				auto condition_expression = if_else_syntax_node->condition_expression;
				auto condition_type = condition_expression->result->type;
				if (condition_type->kind != TypeKind::Atomic) {
					String error_message;
					zero(&error_message);
					write(&error_message, *condition_expression->site, &heap_stack);
					write(&error_message, "Type error: expected integer type in if condition.\n", &heap_stack);
					print(error_message);
					return false;
				}
				
				if (!(validate_control_flow_nodes(if_else_control_flow_node->if_control_flow_nodes, &if_else_syntax_node->if_body)
					  && validate_control_flow_nodes(if_else_control_flow_node->else_control_flow_nodes, &if_else_syntax_node->else_body))) {
					return false;
				}
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
	}
	
	return true;
}

internal bool validate_function_semantics(SyntaxNodeFunction* function) {
	if (!validate_control_flow_nodes(function->control_flow_nodes, &function->body)) {
		return false;
	}
	
	//@todo rewrite this control flow code to operate on an actual control flow graph
#if 0
	//Check for unreachable code
	auto return_found = false;
	SyntaxNode* maybe_return_statement = nullptr;
	fox_for (statement_index, function->body.length) {
		maybe_return_statement = (SyntaxNodeReturn*)function->body[statement_index];
		if (maybe_return_statement->kind == SyntaxNodeKind::Return) {
			return_found = true;
			if (statement_index != function->body.length - 1) {
				auto next_statement = function->body[statement_index + 1];
				String error_message;
				zero(&error_message);
				write(&error_message, *next_statement->site, &heap_stack);
				write(&error_message, "Warning: unreachable code.\n", &heap_stack);
				print(error_message);
			}
		}
	}
	
	if (function->return_types && !return_found) {
		String error_message;
		zero(&error_message);
		write(&error_message, *maybe_return_statement->site, &heap_stack);
		write(&error_message, "Semantic error: function should end with return statement.\n", &heap_stack);
		print(error_message);
		return false;
	}
#endif
	
	return true;
}

//Semantic validation is responsible for determining the types of expressions and statements
//and ensuring the rules of the language are upheld
internal bool validate_semantics(LinearizerContext* linearizer) {
	//Generate types from linearized types
	fox_for (type_index, linearizer->linearized_types.length) {
		auto linearized_type = linearizer->linearized_types[type_index];
		
		SyntaxNode* type_node = nullptr;
		fox_for (type_node_index, linearized_type->type_nodes.length) {
			type_node = linearized_type->type_nodes[type_node_index];
			switch (type_node->kind) {
				case SyntaxNodeKind::AtomicType: {
					auto atomic_type_node = (SyntaxNodeAtomicType*)type_node;
					type_node->type_result = &atomic_type_node->atomic_type;
				} break;
				
				case SyntaxNodeKind::FixedLengthArrayType: {
					auto array_type_node = (SyntaxNodeFixedLengthArrayType*)type_node;
					auto array_type = create_type(FixedLengthArray);
					array_type->element_type = array_type_node->element_type_node->type_result;
					array_type->length = array_type_node->length;
					array_type_node->type_result = array_type;
				} break;
				
				default: {
					fox_unreachable;
				} break;
			}
		}
		
		*linearized_type->result = type_node->type_result;
	}
	
	fox_for (function_index, linearizer->functions.length) {
		auto function = linearizer->functions[function_index];
		if (!validate_function_semantics(function)) {
			return false;
		}
	}
	
	return true;
}

#if 0
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

internal void compile_type_prefix_c(String* c_code, Type* type) {
	switch (type->kind) {
		case TypeKind::Atomic: {
			auto atomic_type = (TypeAtomic*)type;
			if (!atomic_type->is_signed) {
				write(c_code, "u", &heap_stack);
			}
			write(c_code, "int", &heap_stack);
			write_uint(c_code, size_in_bits(*atomic_type), &heap_stack);
			write(c_code, "_t", &heap_stack);
		} break;
		
		case TypeKind::FixedLengthArray: {
			auto array_type = (TypeFixedLengthArray*)type;
			compile_type_prefix_c(c_code, array_type->element_type);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

internal void compile_type_suffix_c(String* c_code, Type* type) {
	switch (type->kind) {
		case TypeKind::Atomic: {
			//do nothing
		} break;
		
		case TypeKind::FixedLengthArray: {
			auto array_type = (TypeFixedLengthArray*)type;
			write(c_code, "[", &heap_stack);
			write_uint(c_code, array_type->length, &heap_stack);
			write(c_code, "]", &heap_stack);
			compile_type_suffix_c(c_code, array_type->element_type);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

internal void compile_intermediate_value_open_c(String* c_code, u32 intermediate_id, Type* intermediate_type) {
	compile_type_prefix_c(c_code, intermediate_type);
	write(c_code, " ", &heap_stack);
	compile_intermediate_constant_name_c(c_code, intermediate_id);
	compile_type_suffix_c(c_code, intermediate_type);
	write(c_code, " = ", &heap_stack); //@bug this won't assign to arrays correctly
}

internal void compile_intermediate_value_open_c(String* c_code, CBackendValue backend_value, Type* intermediate_type) {
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
	//@bug support multiple returns
	fox_assert(function->return_types.length == 1);
	
	//@bug how do i even deal with suffixes here
	compile_type_prefix_c(c_code, function->return_types[0]);
	write(c_code, " ", &heap_stack);
	write(c_code, function->identifier, &heap_stack);
	write(c_code, "(", &heap_stack);
	
	fox_for (argument_index, function->arguments.length) {
		auto argument = &function->arguments[argument_index];
		compile_type_prefix_c(c_code, argument->value->type);
		write(c_code, " ", &heap_stack);
		write(c_code, argument->identifier, &heap_stack);
		compile_type_suffix_c(c_code, argument->value->type);
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
			compile_type_prefix_c(c_code, declaration->type);
			write(c_code, " ", &heap_stack);
			write(c_code, declaration->identifier, &heap_stack);
			compile_type_suffix_c(c_code, declaration->type);
			//@bug correct initializer
			if (declaration->type->kind == TypeKind::Atomic) {
				write(c_code, " = 0;\n", &heap_stack);
			} else {
				fox_assert(declaration->type->kind == TypeKind::FixedLengthArray);
				write(c_code, " = {0};\n", &heap_stack);
			}
		}
	}
	
	u32 intermediate_id_cursor = 0;
	
	fox_for (syntax_node_index, function->linear_syntax_nodes.length) {
		auto syntax_node = function->linear_syntax_nodes[syntax_node_index];
		
		//Write indent if we will write anything at all
		switch (syntax_node->kind) {
			case SyntaxNodeKind::BinaryMathOperation:
			case SyntaxNodeKind::Assignment:
			case SyntaxNodeKind::Return:
			case SyntaxNodeKind::ArrayAccess:
			case SyntaxNodeKind::IntegerLiteral:
			case SyntaxNodeKind::StringLiteral: {
				write_indent(c_code, 1, &heap_stack);
			} break;
			
			case SyntaxNodeKind::Identifier:
			case SyntaxNodeKind::Declaration: {
				//Do nothing
			} break;
			
			case SyntaxNodeKind::Function:
			case SyntaxNodeKind::AtomicType:
			case SyntaxNodeKind::FixedLengthArrayType:
			default: {
				fox_unreachable;
			} break;
		}
		
		switch (syntax_node->kind) {
			case SyntaxNodeKind::BinaryMathOperation: {
				auto binary_math_operation = (SyntaxNodeBinaryMathOperation*)syntax_node;
				binary_math_operation->result->backend_value = c_backend_value_from_intermediate_id(++intermediate_id_cursor);
				compile_intermediate_value_open_c(c_code, binary_math_operation->result);
				compile_value_c(c_code, binary_math_operation->operands[0]->result);
				write(c_code, " ", &heap_stack);
				write(c_code, keyword_strings[(fuint)binary_math_operation->operator_keyword], &heap_stack);
				write(c_code, " ", &heap_stack);
				compile_value_c(c_code, binary_math_operation->operands[1]->result);
				compile_intermediate_value_close_c(c_code);
			} break;
			
			case SyntaxNodeKind::Assignment: {
				auto assignment = (SyntaxNodeAssignment*)syntax_node;
				auto type = assignment->result->type;
				if (type->kind == TypeKind::Atomic) {
					compile_value_c(c_code, assignment->destination_expression->result);
					write(c_code, " = ", &heap_stack);
					compile_value_c(c_code, assignment->source_expression->result);
					write(c_code, ";\n", &heap_stack);
				} else {
					fox_assert(type->kind == TypeKind::FixedLengthArray);
					write(c_code, "memmove(", &heap_stack);
					compile_value_c(c_code, assignment->destination_expression->result);
					write(c_code, ", ", &heap_stack);
					compile_value_c(c_code, assignment->source_expression->result);
					write(c_code, ", ", &heap_stack);
					write_uint(c_code, get_size(type), &heap_stack);
					write(c_code, ");\n", &heap_stack);
				}
			} break;
			
			case SyntaxNodeKind::Return: {
				//@bug cant return arrays correctly
				auto return_node = (SyntaxNodeReturn*)syntax_node;
				write(c_code, "return ", &heap_stack);
				compile_value_c(c_code, return_node->return_expression->result);
				write(c_code, ";\n", &heap_stack);
			} break;
			
			case SyntaxNodeKind::ArrayAccess: {
				auto array_access = (SyntaxNodeArrayAccess*)syntax_node;
				array_access->result->backend_value = c_backend_value_from_intermediate_id(++intermediate_id_cursor);
				compile_intermediate_value_open_c(c_code, array_access->result);
				compile_value_c(c_code, array_access->array_expression->result);
				write(c_code, "[", &heap_stack);
				compile_value_c(c_code, array_access->index_expression->result);
				write(c_code, "]", &heap_stack);
				compile_intermediate_value_close_c(c_code);
			} break;
			
			case SyntaxNodeKind::IntegerLiteral: {
				auto integer_literal = (SyntaxNodeIntegerLiteral*)syntax_node;
				integer_literal->result->backend_value = c_backend_value_from_intermediate_id(++intermediate_id_cursor);
				compile_intermediate_value_open_c(c_code, integer_literal->result);
				write_uint(c_code, integer_literal->integer, &heap_stack);
				compile_intermediate_value_close_c(c_code);
			} break;
			
			case SyntaxNodeKind::StringLiteral: {
				auto string_node = (SyntaxNodeStringLiteral*)syntax_node;
				string_node->result->backend_value = c_backend_value_from_intermediate_id(++intermediate_id_cursor);
				compile_intermediate_value_open_c(c_code, string_node->result);
				write(c_code, "\"", &heap_stack);
				write(c_code, string_node->string, &heap_stack); //@bug we need to escape this
				write(c_code, "\"", &heap_stack);
				compile_intermediate_value_close_c(c_code);
			} break;
			
			case SyntaxNodeKind::Identifier:
			case SyntaxNodeKind::Declaration: {
				//Do nothing
			} break;
			
			case SyntaxNodeKind::Function:
			case SyntaxNodeKind::AtomicType:
			case SyntaxNodeKind::FixedLengthArrayType:
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
#endif

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
			//auto c_code = compile_c(&linearizer);
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