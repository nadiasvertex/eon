//
// Created by christopher on 4/26/15.
//

#ifndef EON_CONTEXT_H
#define EON_CONTEXT_H

#include "../row_id.h"

namespace eon {
    namespace stream {

        struct context {
            row_id rid;
            uint64_t segment_index;
        };

    }
}


#endif //EON_CONTEXT_H
