
# asm.py
# By Ben Anderson
# December 2018


class AsmGenerator:
    """
    Responsible for generating human-readable assembly code (as a string) from
    the IR.
    """

    def __init__(self, ir):
        """
        Create a generator that can write assembly code from an IR graph.
        :param tokens: The IR graph to generate the assembly code from.
        """
        self.ir = ir

    def gen(self):
        """
        Generate assembly code from the IR.
        :return: The assembly code as a string.
        """
        return None
