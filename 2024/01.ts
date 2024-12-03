import { readFileSync } from 'fs';
import { resolve } from 'path';

// Type definitions
type LocationPair = [number[], number[]];

// Read input file
function readInput(): string[] {
    const filePath = resolve(__dirname, '01.input');
    return readFileSync(filePath, 'utf-8')
        .trim()
        .split('\n');
}

// Parse raw locations into two arrays
function parseLocations(lines: string[]): LocationPair {
    const locationsA: number[] = [];
    const locationsB: number[] = [];
    
    lines.forEach(line => {
        const [a, b] = line.trim().split(/\s+/).map(Number);
        locationsA.push(a);
        locationsB.push(b);
    });
    
    return [locationsA, locationsB];
}

// Calculate sum of absolute differences
function calculateDiffs(locationsA: number[], locationsB: number[]): number {
    const sortedA = [...locationsA].sort((a, b) => a - b);
    const sortedB = [...locationsB].sort((a, b) => a - b);
    
    return sortedA.reduce((sum, _, index) => 
        sum + Math.abs(sortedA[index] - sortedB[index]), 0);
}

// Calculate frequency map for locations
function calculateFrequencies(locations: number[]): Map<number, number> {
    return locations.reduce((freq, location) => {
        freq.set(location, (freq.get(location) || 0) + 1);
        return freq;
    }, new Map<number, number>());
}

// Calculate similarity score
function calculateSimilarityScore(
    locationsA: number[], 
    frequenciesB: Map<number, number>
): number {
    return locationsA.reduce((score, location) => {
        const frequency = frequenciesB.get(location) || 0;
        return score + (frequency * location);
    }, 0);
}

function main(): void {
    try {
        // Read and parse input
        const lines = readInput();
        const [locationsA, locationsB] = parseLocations(lines);
        
        // Sort locations
        const sortedA = [...locationsA].sort((a, b) => a - b);
        const sortedB = [...locationsB].sort((a, b) => a - b);
        
        // Part 1: Calculate sum of differences
        const sumDiffs = calculateDiffs(sortedA, sortedB);
        console.log(`Sum of differences: ${sumDiffs}`);
        
        // Part 2: Calculate similarity score
        const frequenciesB = calculateFrequencies(sortedB);
        const similarityScore = calculateSimilarityScore(sortedA, frequenciesB);
        console.log(`Similarity score: ${similarityScore}`);
        
    } catch (error) {
        console.error('Error processing file:', error);
        process.exit(1);
    }
}

// Run the program
main(); 