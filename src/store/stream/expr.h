//
// Created by christopher on 4/26/15.
//

#ifndef EON_EXPR_H
#define EON_EXPR_H

#include <memory>

#include "../scalar_data.h"
#include "op.h"

namespace eon {
    namespace query {

        enum class expr_type {
            unary,
            binary,
            scalar_literal,
            column_value
        };

        struct expr {
        public:
            expr_type type;
        protected:
            expr(expr_type _type):type(_type) {}
        };

        struct unary_expr : public expr {
            op op_code;
            std::unique_ptr<expr> child;
            unary_expr(op _opcode, expr *_child) : expr(expr_type::unary), op_code(_opcode), child(_child) {}
        };

        struct binary_expr : public expr {
            op op_code;
            std::unique_ptr<expr> left, right;
            binary_expr(op _opcode, expr *_left, expr *_right) : expr(expr_type::binary), op_code(_opcode), left(_left), right(_right) {}
        };

        struct literal_expr : public expr {
            store::scalar_data data;
            literal_expr(store::scalar_data _data) : expr(expr_type::scalar_literal), data(_data) { }
        };

        /**
         * This means the value of the current row of the current column.
         */
        struct column_expr : public expr {
            column_expr() : expr(expr_type::column_value) { }
        };
    }
}

#endif //EON_EXPR_H
