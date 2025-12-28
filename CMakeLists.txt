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
  src/hir.cpp
  src/layout.cpp
  src/lower_mir.cpp
  src/main.cpp
  src/mir.cpp
  src/mir_interp.cpp
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
add_test(NAME compile_modules_use            COMMAND cogc --emit-exe test_00.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/modules_use/main.cg)
add_test(NAME compile_comptime_consts        COMMAND cogc --emit-exe test_01.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/comptime_consts/main.cg)
add_test(NAME compile_basic_codegen          COMMAND cogc --emit-exe test_02.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/basic_codegen/main.cg)
add_test(NAME compile_inherent_methods       COMMAND cogc --emit-exe test_03.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/inherent_methods/main.cg)
add_test(NAME compile_control_flow           COMMAND cogc --emit-exe test_04.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/control_flow/main.cg)
add_test(NAME compile_ffi_minimal            COMMAND cogc --emit-exe test_05.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/ffi_minimal/main.cg)
add_test(NAME compile_ffi_tags_strings       COMMAND cogc --emit-exe test_06.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/ffi_tags_strings/main.cg)
add_test(NAME compile_match_or_patterns      COMMAND cogc --emit-exe test_07.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/match_or_patterns/main.cg)
add_test(NAME compile_pointers_and_deref     COMMAND cogc --emit-exe test_08.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/pointers_and_deref/main.cg)
add_test(NAME compile_size_align_builtins    COMMAND cogc --emit-exe test_09.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/size_align_builtins/main.cg)
add_test(NAME compile_hello_world            COMMAND cogc --emit-exe test_10.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/hello_world/main.cg)
add_test(NAME compile_comptime_calls         COMMAND cogc --emit-exe test_11.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/comptime_function_calls/main.cg)
add_test(NAME compile_comptime_params        COMMAND cogc --emit-exe test_12.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/comptime_parameters/main.cg)
add_test(NAME compile_tuple_structs          COMMAND cogc --emit-exe test_13.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/tuple_structs/main.cg)
add_test(NAME compile_function_pointers      COMMAND cogc --emit-exe test_14.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/function_pointers/main.cg)
add_test(NAME compile_tuples                 COMMAND cogc --emit-exe test_15.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/tuples/main.cg)
add_test(NAME compile_never_type             COMMAND cogc --emit-exe test_16.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/never_type/main.cg)
add_test(NAME compile_match_exhaustiveness   COMMAND cogc --emit-exe test_17.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/match_exhaustiveness/main.cg)
add_test(NAME compile_main_unit              COMMAND cogc --emit-exe test_18.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/main_unit/main.cg)
add_test(NAME compile_main_export_c          COMMAND cogc --emit-exe test_21.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/main_export_c/main.cg)
add_test(NAME compile_numerics_arrays_floats COMMAND cogc --emit-exe test_22.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/numerics_arrays_floats/main.cg)
add_test(NAME compile_crc32_tool             COMMAND cogc --emit-exe test_23.bin ${CMAKE_CURRENT_SOURCE_DIR}/examples/crc32_tool/main.cg)

# v0.0.20: IR dumps (smoke)
add_test(NAME emit_hir_smoke  COMMAND cogc --emit-hir test_emit_00.hir ${CMAKE_CURRENT_SOURCE_DIR}/examples/comptime_parameters/main.cg)
add_test(NAME emit_mir_smoke  COMMAND cogc --emit-mir test_emit_01.mir ${CMAKE_CURRENT_SOURCE_DIR}/examples/comptime_parameters/main.cg)
add_test(NAME emit_mirl_smoke COMMAND cogc --emit-mir-after lower test_emit_02.mir ${CMAKE_CURRENT_SOURCE_DIR}/examples/comptime_parameters/main.cg)

add_test(NAME fail_non_exhaustive_match   COMMAND cogc --emit-exe test_neg_00.bin ${CMAKE_CURRENT_SOURCE_DIR}/test/negative/non_exhaustive_match.cg)
set_tests_properties(fail_non_exhaustive_match PROPERTIES WILL_FAIL TRUE)
add_test(NAME fail_int_literal_does_not_fit COMMAND cogc --emit-exe test_neg_01.bin ${CMAKE_CURRENT_SOURCE_DIR}/test/negative/int_literal_does_not_fit.cg)
set_tests_properties(fail_int_literal_does_not_fit PROPERTIES WILL_FAIL TRUE)
add_test(NAME fail_main_bad_signature    COMMAND cogc --emit-exe test_neg_02.bin ${CMAKE_CURRENT_SOURCE_DIR}/test/negative/main_bad_signature.cg)
set_tests_properties(fail_main_bad_signature PROPERTIES WILL_FAIL TRUE)
add_test(NAME fail_main_args_requires_export COMMAND cogc --emit-exe test_neg_03.bin ${CMAKE_CURRENT_SOURCE_DIR}/test/negative/main_args_requires_export.cg)
set_tests_properties(fail_main_args_requires_export PROPERTIES WILL_FAIL TRUE)
add_test(NAME fail_no_main              COMMAND cogc --emit-exe test_neg_04.bin ${CMAKE_CURRENT_SOURCE_DIR}/test/negative/no_main.cg)
set_tests_properties(fail_no_main PROPERTIES WILL_FAIL TRUE)
