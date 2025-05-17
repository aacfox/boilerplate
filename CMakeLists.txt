cmake_minimum_required(VERSION 4.0.1)
set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD "a9e1cf81-9932-4810-974b-6eccaf14e457")

project(boilerplate VERSION 2.3.3.1) #These numbers should change only
#                           | | | |   completely independently from each other
#                           | | |  `cosmetic changes (affects neither performance nor functionality)
#                           | |  `implementation changes (affects performance)
#                           |  `functionality changes (adding/removing utils)
#                            ` ABI breaks, other 'too big to fit in lesser radix' changes  
set(CMAKE_CXX_MODULE_STD 1)

add_library(boilerplate STATIC)
target_sources(boilerplate PUBLIC FILE_SET CXX_MODULES FILES boilerplate.cxx)
target_compile_features(boilerplate PUBLIC cxx_std_26)

include(FetchContent)
FetchContent_Declare(
  GSL
  GIT_REPOSITORY "https://github.com/microsoft/GSL"
  GIT_TAG "origin/main"
  GIT_SHALLOW ON)
FetchContent_MakeAvailable(GSL)
target_link_libraries(boilerplate PUBLIC GSL)
