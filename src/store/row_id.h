//
// Created by christopher on 4/24/15.
//

#ifndef EON_ROWID_H
#define EON_ROWID_H


#include <cstdint>
#include <memory>

class row_id {
    uint64_t id;
    row_id(uint64_t _id):id(_id) {}

public:
    static row_id from_index(uint64_t index) {
        return row_id(index);
    }

    uint64_t get_index(const row_id &base) const {
        return base.id - id;
    }
};


#endif //EON_ROWID_H
