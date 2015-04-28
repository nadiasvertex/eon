//
// Created by christopher on 4/24/15.
//

#include <stack>
#include "mem_segment.h"

using namespace std;

namespace eon {
    namespace store {

        template<typename T>
        scalar_data mem_segment<T>::get(uint64_t index) {
            scalar_data d;
            _get(d, data[index]);
            return d;
        }

        template<typename T>
        uint64_t mem_segment<T>::put(const scalar_data &item) {
            switch (item.type) {
                case data_type::i8:
                    return _put(item.value.i8);
                case data_type::i16:
                    return _put(item.value.i16);
                case data_type::i32:
                    return _put(item.value.i32);
                case data_type::i64:
                    return _put(item.value.i64);
                case data_type::timestamp:
                    return _put(item.value.ts);
                case data_type::text:
                    return _put(item.value.st);
                default:
                    throw new std::invalid_argument("Unsupported data type in mem_segment.");
            }
        }

        template<typename T>
        T mem_segment<T>::_convert(const scalar_data &item) {
            switch (item.type) {
                case data_type::i8:
                    return item.value.i8;
                case data_type::i16:
                    return item.value.i16;
                case data_type::i32:
                    return item.value.i32;
                case data_type::i64:
                    return item.value.i64;
                case data_type::timestamp:
                    return item.value.ts;
                case data_type::text:
                    return std::stoi(*item.value.st);
                default:
                    throw new std::invalid_argument("Unsupported data type in mem_segment::convert.");
            }
        }

        template<>
        store::string_type mem_segment<store::string_type>::_convert(const scalar_data &item) {
            switch (item.type) {
                case data_type::i8:
                    return make_shared<string>(to_string(item.value.i8));
                case data_type::i16:
                    return make_shared<string>(to_string(item.value.i16));
                case data_type::i32:
                    return make_shared<string>(to_string(item.value.i32));
                case data_type::i64:
                    return make_shared<string>(to_string(item.value.i64));
                case data_type::timestamp:
                    return make_shared<string>(to_string(item.value.ts));
                case data_type::text:
                    return item.value.st;
                default:
                    throw new std::invalid_argument("Unsupported data type in mem_segment::convert.");
            };
        }

        template<typename T>
        void mem_segment<T>::compile(std::shared_ptr<eon::query::expr> predicate) {
            compiled_program_t compiled;
            _compile(compiled, predicate.get());
        }

        template<typename T>
        void mem_segment<T>::_compile(compiled_program_t &compiled, eon::query::expr *e) {
            switch (e->type) {
                case query::expr_type::scalar_literal: {
                    auto le = static_cast<query::literal_expr *>(e);
                    T value{_convert(le->data)};
                    compiled.push_back([=](auto &ctx, auto &s) {
                        s.push(value);
                    });
                } break;

                case query::expr_type::column_value: {
                    auto le = static_cast<query::column_expr *>(e);
                    compiled.push_back([this](auto &ctx, auto &s) {
                        s.push(this->data.at(ctx.segment_index));
                    });
                } break;

                case query::expr_type::unary: {
                    auto ue = static_cast<query::unary_expr *>(e);
                    auto op_code = ue->op_code;

                    _compile(compiled, ue->child.get());

                    compiled.push_back([=](auto &ctx, auto &s) {
                        T value{s.top()};
                        s.pop();

                        switch (op_code) {
                            default:
                                throw std::runtime_error{"Invalid opcode in predicate program for unary_expr."};
                                break;

                            case query::op::boolean_not:
                                s.push(!value);
                                break;
                        }
                    });
                } break;

                case query::expr_type::binary: {
                    auto be = static_cast<query::binary_expr *>(e);
                    auto op_code = be->op_code;

                    _compile(compiled, be->left.get());
                    _compile(compiled, be->right.get());

                    compiled.push_back([=](auto &ctx, auto &s) {
                        T lvalue{s.top()};
                        s.pop();
                        T rvalue{s.top()};
                        s.pop();

                        switch (op_code) {
                            default:
                                throw std::runtime_error{"Invalid opcode in predicate program for binary_expr."};
                                break;

                            case query::op::boolean_and:
                                s.push(lvalue && rvalue);
                                break;
                            case query::op::boolean_or:
                                s.push(lvalue || rvalue);
                                break;
                            case query::op::eq:
                                s.push(lvalue == rvalue);
                                break;
                            case query::op::neq:
                                s.push(lvalue != rvalue);
                                break;
                            case query::op::lt:
                                s.push(lvalue < rvalue);
                                break;
                            case query::op::gt:
                                s.push(lvalue > rvalue);
                                break;
                            case query::op::lte:
                                s.push(lvalue <= rvalue);
                                break;
                            case query::op::gte:
                                s.push(lvalue >= rvalue);
                                break;
                        }
                    });
                } break;

            }
        }
    }
}