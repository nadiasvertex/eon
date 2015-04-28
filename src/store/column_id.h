//
// Created by christopher on 4/25/15.
//

#ifndef EON_COLUMN_ID_H
#define EON_COLUMN_ID_H

#include <cstdint>

namespace eon {
    namespace store {
        class column_id {
            uint16_t id;
            column_id(uint16_t _id):id(_id) {}

        public:
            static column_id from_index(uint16_t index) {
                return column_id(index);
            }

            uint16_t get_index() const {
                return id;
            }
        };
    }
}

#endif //EON_COLUMN_ID_H
