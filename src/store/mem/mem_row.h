//
// Created by christopher on 4/24/15.
//

#ifndef EON_MEMROW_H
#define EON_MEMROW_H

#include <cstdint>
#include <string>
#include <vector>

#include "../row_id.h"
#include "../base/row.h"
#include "mem_segment.h"

namespace eon {
    namespace store {

        class mem_row : public row {
            static row_store_t* _get_row_store(const row_type_t &row_type);

            virtual row_value_t get_row_value(uint64_t index);
            virtual col_value_t get_col_value(uint64_t row_index, uint16_t column_index);

        public:
            mem_row(row_id const &_base, const row_type_t &row_type) : row(_base, _get_row_store(row_type)) { }

        };

    }
}


#endif //EON_MEMROW_H
