class Fitswebqlse < Formula
  desc "This is a high-performance FORTRAN & C FITSWEBQL SE (Supercomputer Edition)"
  homepage "https://github.com/jvo203/FITSWEBQLSE"
  url "https://github.com/jvo203/FITSWEBQLSE/archive/refs/tags/v5.0.7.tar.gz"
  sha256 "1c9b69d01cd343bb45d94925ec6997ca349841155372e4e2f34a644a9f645966"
  license "MIT"

  depends_on "gcc@12" => :build
  depends_on "cfitsio"
  depends_on "czmq"
  depends_on "glib"
  depends_on "ispc"
  depends_on "jemalloc"
  depends_on "libmicrohttpd"
  depends_on "libpq"
  depends_on "lz4"
  depends_on "pkg-config"
  depends_on "x265"

  def install
    ENV.deparallelize
    system "make"
    bin.install "fitswebqlse"
  end

  test do
    assert_match "FITSWEBQLSE", shell_output("#{bin}/fitswebqlse -h")
  end
end
