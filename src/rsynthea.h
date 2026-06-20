#pragma once

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <optional>
#include <memory>
#include <cmath>

// ── Attribute value type ─────────────────────────────────────────────────────
// Represents any value that can live in a PersonRecord field or patient
// attribute: null (monostate), bool, double, or std::string.
using AttrVal = std::variant<std::monostate, bool, double, std::string>;

inline bool attr_is_null(const AttrVal& v)   { return std::holds_alternative<std::monostate>(v); }
inline bool attr_is_bool(const AttrVal& v)   { return std::holds_alternative<bool>(v); }
inline bool attr_is_double(const AttrVal& v) { return std::holds_alternative<double>(v); }
inline bool attr_is_string(const AttrVal& v) { return std::holds_alternative<std::string>(v); }

inline bool        attr_bool(const AttrVal& v)   { return std::get<bool>(v); }
inline double      attr_double(const AttrVal& v)  { return std::get<double>(v); }
inline const std::string& attr_string(const AttrVal& v) { return std::get<std::string>(v); }

// Sentinel used for module_current to mark a terminal module
static const std::string TERMINAL = "__terminal__";

// ── CppCode ─────────────────────────────────────────────────────────────────
// Fundamental coded concept (SNOMED, LOINC, RxNorm, etc.)
struct CppCode {
    std::string system;
    std::string code;
    std::string display;
};

// ── Forward declarations ──────────────────────────────────────────────────────
struct PersonRecord;
struct CppModule;
struct CppState;
