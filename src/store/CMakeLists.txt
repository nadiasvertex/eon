cmake_minimum_required(VERSION 3.1)
project(store)

set(SOURCE_FILES
    main.cpp
    mem/mem_segment.cpp mem/mem_segment.h
    base/segment.cpp base/segment.h
    base/row.cpp base/row.h
    mem/mem_row.cpp mem/mem_row.h
    row_id.cpp row_id.h
    column_id.cpp column_id.h
    scalar_data.cpp scalar_data.h
)

add_executable(store ${SOURCE_FILES} stream/predicate.cpp stream/predicate.h stream/op.h stream/expr.cpp stream/expr.h stream/context.cpp stream/context.h)