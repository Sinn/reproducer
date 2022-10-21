
workspace(name = "foo")

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

# GoogleTest/GoogleMock framework. Used by most unit-tests.
git_repository(
    name = "googletest",
    commit = "58d77fa8070e8cec2dc1ed015d66b454c8d78850",
    remote = "https://github.com/google/googletest.git",
    shallow_since = "1656350095 -0400",
)

