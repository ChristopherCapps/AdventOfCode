from typing import List, Dict, Tuple
from pathlib import Path

# Read input file
def read_input() -> List[str]:
    return Path('01.input').read_text().splitlines()

# Parse raw locations into two lists
def parse_locations(lines: List[str]) -> Tuple[List[int], List[int]]:
    locations_a: List[int] = []
    locations_b: List[int] = []
    
    for line in lines:
        # Split line and convert to integers
        a, b = map(int, line.split())
        locations_a.append(a)
        locations_b.append(b)
    
    return locations_a, locations_b

# Calculate sum of absolute differences
def calculate_diffs(locations_a: List[int], locations_b: List[int]) -> int:
    locations_a_sorted = sorted(locations_a)
    locations_b_sorted = sorted(locations_b)
    
    diffs = [abs(a - b) for a, b in zip(locations_a_sorted, locations_b_sorted)]
    return sum(diffs)

# Calculate frequency map for locations_b
def calculate_frequencies(locations: List[int]) -> Dict[int, int]:
    frequencies: Dict[int, int] = {}
    for location in locations:
        frequencies[location] = frequencies.get(location, 0) + 1
    return frequencies

# Calculate similarity score
def calculate_similarity_score(locations_a: List[int], frequencies_b: Dict[int, int]) -> int:
    score = 0
    for location in locations_a:
        if location in frequencies_b:
            score += frequencies_b[location] * location
    return score

def main():
    # Read and parse input
    lines = read_input()
    locations_a, locations_b = parse_locations(lines)
    
    # Sort locations
    locations_a_sorted = sorted(locations_a)
    locations_b_sorted = sorted(locations_b)
    
    # Part 1: Calculate sum of differences
    sum_diffs = calculate_diffs(locations_a_sorted, locations_b_sorted)
    print(f"Sum of differences: {sum_diffs}")
    
    # Part 2: Calculate similarity score
    frequencies_b = calculate_frequencies(locations_b_sorted)
    similarity_score = calculate_similarity_score(locations_a_sorted, frequencies_b)
    print(f"Similarity score: {similarity_score}")

if __name__ == "__main__":
    main() 