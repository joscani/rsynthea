#pragma once
#include "person_record.h"
#include "conditions.h"

// Evaluate a pre-compiled condition tree against the current PersonRecord.
// Returns true if the condition is satisfied.
bool evaluate_condition_cpp(const CppCond* cond, PersonRecord& rec);

// Compare two scalars (numeric context)
inline bool compare_num(double left, CompOp op, double right) {
    switch (op) {
    case CompOp::Eq: return left == right;
    case CompOp::Ne: return left != right;
    case CompOp::Lt: return left <  right;
    case CompOp::Le: return left <= right;
    case CompOp::Gt: return left >  right;
    case CompOp::Ge: return left >= right;
    default: return false;
    }
}

// Compare two AttrVal values (heterogeneous)
bool compare_attr(const AttrVal& left, CompOp op, const AttrVal& right);
