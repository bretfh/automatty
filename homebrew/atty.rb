# This formula belongs in the tap bretfh/homebrew-automatty, at Formula/atty.rb.
# The version and the sha256 lines track a tagged release: copy them from the
# release's SHA256SUMS.
class Atty < Formula
  desc "Terminal emulator and multiplexer that knows what the program in each pane is doing"
  homepage "https://github.com/bretfh/automatty"
  version "0.1.0"
  license "GPL-3.0-or-later"

  on_macos do
    on_arm do
      url "https://github.com/bretfh/automatty/releases/download/v#{version}/atty-v#{version}-darwin-arm64.tar.gz"
      sha256 "bc0e626d09eb983c3907e989c3334bd6bfc26045aaa10430adf5554f70ed9c69"
    end
    on_intel do
      url "https://github.com/bretfh/automatty/releases/download/v#{version}/atty-v#{version}-darwin-x86_64.tar.gz"
      sha256 "bc313152c445f3b12045105674967b64b9f7905ef0809430e6a7ad9599d96cd6"
    end
  end

  on_linux do
    on_arm do
      url "https://github.com/bretfh/automatty/releases/download/v#{version}/atty-v#{version}-linux-aarch64.tar.gz"
      sha256 "fe3155a8e024f774293447df833a933d31f496039820c87f6d766861834e2a2f"
    end
    on_intel do
      url "https://github.com/bretfh/automatty/releases/download/v#{version}/atty-v#{version}-linux-x86_64.tar.gz"
      sha256 "955e3fa9135f577b640360b21431f81190db340f9a10d6ad4db696627e3e8171"
    end
  end

  def install
    bin.install "atty"
  end

  test do
    assert_match "atty v#{version}", shell_output("#{bin}/atty version")
  end
end
