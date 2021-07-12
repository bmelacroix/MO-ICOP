/*
 * Seed.java
 * Copyright (c) 2016, Robert Gordon University, Aberdeen.
 * All rights reserved.
 */
package jmetal.problems.MO_ICOP;

/**
 * Defines a seed solution.
 */
public final class Seed implements Comparable<Seed> {

	private double fitness;
	private double[] genes;
	private int id;

	/**
	 * Constructs a seed.
	 *
	 * @param g
	 *            the genes
	 */
	public Seed(double[] g) {
		genes = g;
	}

	/**
	 * Returns the fitness of the seed.
	 *
	 * @return the fitness
	 */
	public double getFitness() {
		return fitness;
	}

	/**
	 * Sets the fitness of the seed.
	 *
	 * @param fitness
	 *            the fitness
	 */
	public void setFitness(double fitness) {
		this.fitness = fitness;
	}

	/**
	 * Returns the genes of the seed.
	 *
	 * @return the genes
	 */
	public double[] getGenes() {
		return genes;
	}

	/**
	 * Sets the genes of the seed.
	 *
	 * @param genes
	 *            the genes
	 */
	public void setGenes(double[] genes) {
		this.genes = genes;
	}

	/**
	 * Gets the ID of the seed.
	 *
	 * @return the ID
	 */
	public int getID() {
		return id;
	}

	/**
	 * Sets the ID of the seed.
	 *
	 * @param id
	 *            the ID
	 */
	public void setID(int id) {
		this.id = id;
	}

	// @Override
	// public String toString() {
	// String ret = id + " : " + fitness + " : ";
	// ret += ArrayUtils.print(genes);
	// return ret;
	// }

	public int compareTo(Seed o) {
		if (fitness < o.fitness)
			return -1;
		if (fitness > o.fitness)
			return 1;
		return 0;
	}

}
