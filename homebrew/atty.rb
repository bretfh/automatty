# This formula belongs in the tap bretfh/homebrew-automatty, at Formula/atty.rb.
# The version and the two sha256 lines track a tagged release: after release.yml
# has put the tarballs and SHA256SUMS on the release, copy the sums in.
class Atty < Formula
  desc "Terminal emulator and multiplexer that knows what the program in each pane is doing"
  homepage "https://github.com/bretfh/automatty"
  version "0.1.0"
  license "GPL-3.0-or-later"

  on_arm do
    url "https://github.com/bretfh/automatty/releases/download/v#{version}/atty-v#{version}-darwin-arm64.tar.gz"
    sha256 "0000000000000000000000000000000000000000000000000000000000000000"
  end

  on_intel do
    url "https://github.com/bretfh/automatty/releases/download/v#{version}/atty-v#{version}-darwin-x86_64.tar.gz"
    sha256 "0000000000000000000000000000000000000000000000000000000000000000"
  end

  def install
    bin.install "atty"
  end

  test do
    assert_match "atty v#{version}", shell_output("#{bin}/atty version")
  end
end
