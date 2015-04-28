//
// Created by christopher on 4/24/15.
//

#ifndef EON_SEGMENT_H
#define EON_SEGMENT_H

#include "../scalar_data.h"

namespace eon {
    namespace store {

        class segment {
        public:
            virtual scalar_data get(uint64_t index)=0;
            virtual uint64_t put(const scalar_data& item)=0;
        };

    }
}

#endif //EON_SEGMENT_H