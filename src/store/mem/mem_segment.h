//
// Created by christopher on 4/24/15.
//

#ifndef EON_MEMSTORE_H
#define EON_MEMSTORE_H

#include <vector>
#include <stdexcept>

#include "../base/segment.h"
#include "../scalar_data.h"
#include "../stream/context.h"
#include "../stream/expr.h"

namespace eon {
    namespace store {

        template<typename T>
        class mem_segment : public segment {
            typedef std::vector<std::function<void(stream::context& ctx, std::stack<T> &)>> compiled_program_t;

            std::vector<T> data;

            template<typename V>
            inline void _get(scalar_data &d, T &v) const {
                throw new std::invalid_argument("Unknown scalar type in mem_segment.");
            }

            inline void _get(scalar_data &d, int8_t &v) const {
                d.type = data_type::i8;
                d.value.i8 = v;
            }

            inline void _get(scalar_data &d, int16_t &v) const {
                d.type = data_type::i16;
                d.value.i16 = v;
            }

            inline void _get(scalar_data &d, int32_t &v) const {
                d.type = data_type::i16;
                d.value.i32 = v;
            }

            inline void _get(scalar_data &d, int64_t &v) const {
                d.type = data_type::i16;
                d.value.i64 = v;
            }

            inline void _get(scalar_data &d, uint64_t &v) const {
                d.type = data_type::timestamp;
                d.value.ts = v;
            }

            inline void _get(scalar_data &d, string_type v) const {
                d.type = data_type::text;
                d.value.st = v;
            }

            template<typename V>
            inline uint64_t _put(T v) {
                data.push_back(v);
                return data.size() - 1;
            }

            virtual void _compile(compiled_program_t &compiled, eon::query::expr *e);

            T _convert(const scalar_data &item);


        public:
            mem_segment() { }

            virtual scalar_data get(uint64_t index);

            virtual uint64_t put(const scalar_data &data);

            virtual void compile(std::shared_ptr<eon::query::expr> predicate);
        };

    }
}


#endif //EON_MEMSTORE_H
