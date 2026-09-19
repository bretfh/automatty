# This formula belongs in the tap bretfhorne/homebrew-vtx, at Formula/vtx.rb.
# The url, version and sha256 below track a tagged release; bump them
# together after `make FOREIGN=1 vtx` on macOS produces the tarball
# release.yml uploads.
class Vtx < Formula
  desc "Terminal emulator and multiplexer built on libvtx"
  homepage "https://github.com/bretfhorne/vtx"
  url "https://github.com/bretfhorne/vtx/releases/download/v0.0.1/vtx-macos.tar.gz"
  sha256 "0000000000000000000000000000000000000000000000000000000000000000"
  license "GPL-3.0-or-later"

  def install
    bin.install "vtx"
  end

  test do
    system "#{bin}/vtx", "list"
  end
end
