cmake_minimum_required(VERSION 3.15)
project(blinkc LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Warnings
if (MSVC)
  add_compile_options(/W4 /permissive-)
else()
  add_compile_options(-Wall -Wextra -Wpedantic)
endif()

add_executable(blinkc
  src/main.cpp
)

# If you later split files:
# target_sources(blinkc PRIVATE src/lexer.cpp src/parser.cpp src/codegen.cpp)

