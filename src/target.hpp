#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace cog {

struct Session;

struct TargetLayout {
    std::uint32_t pointer_bits = 64;
    std::uint64_t pointer_size = 8;
    std::uint64_t pointer_align = 8;

    std::uint64_t i8_align = 1;
    std::uint64_t i16_align = 2;
    std::uint64_t i32_align = 4;
    std::uint64_t i64_align = 8;
    std::uint64_t i128_align = 16;
};

struct TargetSpec {
    std::string triple{};
    std::string cpu{};
    std::string features{};
    std::string data_layout{};
    TargetLayout layout{};
};

std::optional<TargetSpec> compute_target_spec(
    Session& session, std::optional<std::string_view> triple);

}  // namespace cog
