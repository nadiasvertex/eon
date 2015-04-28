//
// Created by christopher on 4/25/15.
//

#ifndef EON_SCALAR_DATA_H
#define EON_SCALAR_DATA_H

#include <memory>
#include <cstdint>
#include <string>

namespace eon {
    namespace store {
        enum class data_type {
            none,
            i8,
            i16,
            i32,
            i64,
            decimal,
            date,
            date_time,
            timestamp,
            text
        };

        typedef std::shared_ptr<std::string> string_type;

        struct scalar_data {
            data_type type;
            union {
                int8_t i8;
                int16_t i16;
                int32_t i32;
                int64_t i64;
                uint64_t ts;
                string_type st;
            } value;

            scalar_data():type(data_type::none) {

            }

            scalar_data(const scalar_data& o):type(o.type) {
                switch(type) {
                    case data_type::none:
                        break;
                    case data_type::i8:
                        value.i8 = o.value.i8;
                        break;
                    case data_type::i16:
                        value.i16 = o.value.i16;
                        break;
                    case data_type::i32:
                        value.i32 = o.value.i32;
                        break;
                    case data_type::i64:
                        value.i64 = o.value.i64;
                        break;
                    case data_type::timestamp:
                        value.ts = o.value.ts;
                        break;
                    case data_type::text:
                        value.st = o.value.st;
                        break;
                }
            }

            ~scalar_data() {
                if (type==data_type::text) {

                }
            }
        };


    }
}


#endif //EON_SCALAR_DATA_H
