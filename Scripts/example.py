"""Tiny example module demonstrating a trivial string join function."""


def join_words(word1, word2, word3):
    """Return the three input words joined by single spaces (no trimming or validation)."""
    result = word1 + " " + word2 + " " + word3
    return result


if __name__ == "__main__":
    word1 = "hello"
    word2 = "world"
    word3 = "hello"
    result = join_words(word1, word2, word3)
    print(f"Result: {result}")
