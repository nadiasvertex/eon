//
// Created by christopher on 4/26/15.
//

#ifndef EON_OP_H
#define EON_OP_H
namespace eon {
    namespace query {
        enum class op {
            neq,
            eq,
            lt,
            gt,
            lte,
            gte,
            isnull,
            boolean_not,
            boolean_and,
            boolean_or
        };
    }
}
#endif //EON_OP_H
