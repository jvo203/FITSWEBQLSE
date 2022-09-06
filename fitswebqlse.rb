class Fitswebqlse < Formula
  desc "This is a high-performance FORTRAN & C FITSWEBQL SE (Supercomputer Edition)"
  homepage "https://github.com/jvo203/FITSWEBQLSE"
  url "https://github.com/jvo203/FITSWEBQLSE/archive/refs/tags/v5.0.8.tar.gz"
  sha256 "47b9131484e1df0349d440f326e2db06569be19f030af63415f5ae0b8c92f453"
  license "MIT"

  depends_on "gcc@12" => :build
  depends_on "wget" => :build
  depends_on "cfitsio"
  depends_on "curl"
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
    system "make SHARE=#{share}"
    system "wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db"
    bin.install "fitswebqlse"
    share.install "htdocs"
    share.install "splatalogue_v3.db"
  end

  test do
    assert_match "FITSWEBQLSE", shell_output("#{bin}/fitswebqlse -h")
  end
end
