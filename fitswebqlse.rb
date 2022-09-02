# Documentation: https://docs.brew.sh/Formula-Cookbook
#                https://rubydoc.brew.sh/Formula
# PLEASE REMOVE ALL GENERATED COMMENTS BEFORE SUBMITTING YOUR PULL REQUEST!
class Fitswebqlse < Formula
  desc "a high-performance FORTRAN & C FITSWEBQL SE (Supercomputer Edition)"
  homepage "https://github.com/jvo203/FITSWEBQLSE"
  url "https://github.com/jvo203/FITSWEBQLSE/archive/refs/tags/v5.0.7.tar.gz"
  sha256 "1c9b69d01cd343bb45d94925ec6997ca349841155372e4e2f34a644a9f645966"
  license "MIT"

  depends_on "gcc@12" => :build
  depends_on "ispc"
  depends_on "pkg-config"
  depends_on "glib"
  depends_on "libmicrohttpd"
  depends_on "lz4"
  depends_on "x265"
  depends_on "jemalloc"
  depends_on "czmq"
  depends_on "cfitsio"
  depends_on "libpq"

  def install
    ENV.deparallelize  # if your formula fails when building in parallel
    system "make"
    bin.install "fitswebqlse"
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! For Homebrew/homebrew-core
    # this will need to be a test that verifies the functionality of the
    # software. Run the test with `brew test fitswebqlse`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    # system "false"
    assert_match "FITSWEBQLSE", shell_output("#{bin}/fitswebqlse -h")
  end
end
