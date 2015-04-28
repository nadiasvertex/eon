//
// Created by christopher on 4/24/15.
//

#include "mem_row.h"

namespace eon {
    namespace store {

        row_store_t *mem_row::_get_row_store(const row_type_t &row_type) {
            auto store = new row_store_t{};
            for (auto rt : row_type) {
                switch (rt) {
                    case data_type::i8:
                        store->push_back(std::make_unique<mem_segment<int8_t>>());
                        break;
                    case data_type::i16:
                        store->push_back(std::make_unique<mem_segment<int16_t>>());
                        break;
                    case data_type::i32:
                        store->push_back(std::make_unique<mem_segment<int32_t>>());
                        break;
                    case data_type::i64:
                        store->push_back(std::make_unique<mem_segment<int64_t>>());
                        break;
                    case data_type::timestamp:
                        store->push_back(std::make_unique<mem_segment<uint64_t>>());
                        break;
                    case data_type::text:
                        store->push_back(std::make_unique<mem_segment<std::string *>>());
                        break;
                }
            }

            return store;
        }

        row_value_t mem_row::get_row_value(uint64_t index) {
            std::vector<scalar_data> row;

            for(auto seg : *data) {
                row.push_back(seg->get(index));
            }

            return row;
        }

        col_value_t mem_row::get_col_value(uint64_t row_index, uint16_t column_index) {
            return data->at(column_index)->get(row_index);
        }
    }
}