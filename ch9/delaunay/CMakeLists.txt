PROJECT(delaunay CXX)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -g -std=c++11 -stdlib=libc++")

elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -g -std=c++11 ")

else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ")

endif()

cmake_minimum_required(VERSION 2.8)

ADD_EXECUTABLE(delaunay main.cpp)
TARGET_LINK_LIBRARIES(delaunay)
