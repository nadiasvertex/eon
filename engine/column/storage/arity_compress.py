__author__ = 'Christopher'


class Pilot:
    def __init__(self, threshold=16):
        self.threshold = threshold
        self.values = set()

    def analyze(self, value):
        self.values.add(value)

    def evaluate(self):
        return len(self.values) <= self.threshold

