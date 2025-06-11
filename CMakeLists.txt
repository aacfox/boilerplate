cmake_minimum_required(VERSION 4.0.2)
set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD "a9e1cf81-9932-4810-974b-6eccaf14e457")
set(CMAKE_CXX_MODULE_STD ON)
project(boilerplate VERSION 8.13.13.13)
# ~~~
#                           |  |  |  |   These numbers should change only
#                           |  |  |  |   completely independently from each other
#                           |  |  |  `cosmetic changes (affects neither performance nor functionality)
#                           |  |  `implementation changes (affects performance)
#                           |  `functionality changes (adding/removing utils)
#                           `ABI breaks, other 'too big to fit in lesser radix' changes
# ~~~
include(FetchContent)
FetchContent_Declare(
  GSL
  GIT_REPOSITORY "https://github.com/microsoft/GSL"
  GIT_TAG "origin/main"
  GIT_SHALLOW ON)
FetchContent_MakeAvailable(GSL)

add_library(boilerplate STATIC)
target_compile_features(boilerplate PUBLIC cxx_std_26)
target_sources(boilerplate PUBLIC FILE_SET CXX_MODULES FILES boilerplate.cxx)
target_precompile_headers(boilerplate PUBLIC boil)
target_link_libraries(boilerplate PUBLIC GSL)