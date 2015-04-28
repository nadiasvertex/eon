//
// Created by christopher on 4/24/15.
//

#ifndef EON_ROW_H
#define EON_ROW_H

#include <string>
#include <vector>

#include "segment.h"
#include "../row_id.h"
#include "../column_id.h"
#include "../scalar_data.h"

namespace eon {
    namespace store {
        typedef std::vector<std::unique_ptr<segment>> row_store_t;
        typedef std::vector<scalar_data> row_value_t;
        typedef std::vector<data_type> row_type_t;
        typedef scalar_data col_value_t;

        class row {
        protected:
            std::unique_ptr<row_store_t> data;
            row_id base;

            virtual row_value_t get_row_value(uint64_t index) = 0;
            virtual col_value_t get_col_value(uint64_t row_index, uint16_t column_index)=0;

        public:
            row(row_id _base, row_store_t* _data) : base(_base), data(_data) {

            }

            /**
             * Get the value of a row.
             */
            row_value_t get_row(const row_id &rid) {
                return get_row_value(rid.get_index(base));
            }

            /**
             * Get the value of a column.
             */
            col_value_t get_column(const row_id &rid, const column_id &cid) {
                return get_col_value(rid.get_index(base), cid.get_index());
            }

        };
    }
}

#endif //EON_ROW_H
