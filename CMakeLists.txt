cmake_minimum_required(VERSION 3.13.4)

project(fortran_error_handler)
enable_language(Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(FLAGS "-O3 -fcheck=all -fbackslash -ffpe-trap=zero,invalid,overflow")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(FLAGS "/O3 /fpe:0 /assume:bscc")
endif()

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${FLAGS}")

file(GLOB SOURCES_EXAMPLE src/*.f90 example/example_usage.f90)
file(GLOB SOURCES src/*.f90)
file(GLOB SOURCES_TEST src/*.f90 tests/*.f90)

add_executable(example ${SOURCES_EXAMPLE})
add_executable(test ${SOURCES_TEST})
add_library(feh ${SOURCES})