cmake_minimum_required(VERSION 3.23)

# cpp standard
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# compile db
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# project
project(cog_lang)

find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)

# LLVM (required). Prefer the Homebrew install on macOS without requiring PATH changes.
if(NOT LLVM_DIR AND EXISTS "/opt/homebrew/opt/llvm/lib/cmake/llvm/LLVMConfig.cmake")
  set(LLVM_DIR "/opt/homebrew/opt/llvm/lib/cmake/llvm" CACHE PATH "LLVM CMake package directory")
endif()
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

BISON_TARGET(
  CogParser
  ${CMAKE_CURRENT_SOURCE_DIR}/src/parser.y
  ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/parser.hpp
)

FLEX_TARGET(
  CogLexer
  ${CMAKE_CURRENT_SOURCE_DIR}/src/lexer.l
  ${CMAKE_CURRENT_BINARY_DIR}/lexer.cpp
)

ADD_FLEX_BISON_DEPENDENCY(CogLexer CogParser)

add_executable(
  cogc
  src/ast.cpp
  src/check.cpp
  src/comptime.cpp
  src/diag.cpp
  src/emit_llvm.cpp
  src/layout.cpp
  src/main.cpp
  src/parse.cpp
  src/parse_state.cpp
  src/resolve.cpp
  src/session.cpp
  src/source.cpp
  src/types.cpp
  src/target.cpp
  ${BISON_CogParser_OUTPUTS}
  ${FLEX_CogLexer_OUTPUTS}
)

target_include_directories(cogc PRIVATE src ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories(cogc SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Homebrew LLVM is typically built as a single shared library target (LLVM).
target_link_libraries(cogc PRIVATE LLVM)

if(MSVC)
  target_compile_options(cogc PRIVATE /W4)
else()
  target_compile_options(cogc PRIVATE -Wall -Wextra -Wpedantic)
endif()

enable_testing()
add_test(NAME parse_v0_0_1 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_1.cg)
add_test(NAME parse_v0_0_1_1 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_1_1.cg)
add_test(NAME parse_v0_0_3 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_3/main.cg)
add_test(NAME parse_v0_0_4 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_4/main.cg)
add_test(NAME parse_v0_0_5 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_5/main.cg)
add_test(NAME parse_v0_0_6 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_6/main.cg)
add_test(NAME parse_v0_0_9 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_9/main.cg)
add_test(NAME parse_v0_0_9_methods COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_9_methods/main.cg)
add_test(NAME parse_v0_0_10 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_10/main.cg)
add_test(NAME parse_v0_0_11 COMMAND cogc ${CMAKE_CURRENT_SOURCE_DIR}/examples/v0_0_11/main.cg)
