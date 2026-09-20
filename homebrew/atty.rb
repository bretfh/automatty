# This formula belongs in the tap bretfhorne/homebrew-automatty, at Formula/atty.rb.
# The url, version and sha256 below track a tagged release; bump them
# together after `make FOREIGN=1 atty` on macOS produces the tarball
# release.yml uploads.
class Atty < Formula
  desc "Terminal emulator and multiplexer built on libatty"
  homepage "https://github.com/bretfhorne/automatty"
  url "https://github.com/bretfhorne/automatty/releases/download/v0.0.1/atty-macos.tar.gz"
  sha256 "0000000000000000000000000000000000000000000000000000000000000000"
  license "GPL-3.0-or-later"

  def install
    bin.install "atty"
  end

  test do
    system "#{bin}/atty", "list"
  end
end
