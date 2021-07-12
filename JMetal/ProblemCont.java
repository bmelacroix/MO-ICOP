/*
 * Problem.java
 * Copyright (c) 2016, Robert Gordon University, Aberdeen.
 * All rights reserved.
 */

package jmetal.problems.MO_ICOP;

import java.util.ArrayList;
import java.util.Random;

/**
 * Abstract class for a continuous optimisation problem.
 */
public abstract class ProblemCont {

	protected int dimension;
	protected double optimumFitness;
	protected double[] optimum;
	protected Domain domain;

	protected ArrayList<Seed> sample;

	/**
	 * Constructs a new problem.
	 *
	 * @param dimension
	 *            the number of dimensions
	 * @param optimumFitness
	 *            the global optimum fitness value
	 * @param optimum
	 *            the global optimum candidate
	 * @param domain
	 *            the domain of the problem
	 */
	public ProblemCont(int dimension, double optimumFitness, double[] optimum, Domain domain) {
		this.dimension = dimension;
		this.optimumFitness = optimumFitness;
		this.optimum = optimum;
		this.domain = domain;
	}

	public void createSample(int sampleSize, Random rand) {

		if (sample == null) {
			sample = new ArrayList<Seed>(sampleSize);
		}
		while (sample.size() < sampleSize) {
			double[] randSol = new double[domain.getDimension()];
			for (int i = 0; i < domain.getDimension(); i++) {
				randSol[i] = domain.getMin(i) + rand.nextDouble() * (domain.getMax(i) - domain.getMin(i));
			}
			Seed s = new Seed(randSol);
			s.setFitness(evaluate(randSol));
			sample.add(s);
		}
	}

	/**
	 * Evaluates a candidate solution
	 *
	 * @param sol
	 *            the candidate
	 * @return the fitness
	 */
	abstract public double evaluate(double[] sol);

	public int getDimension() {
		return dimension;
	}

	public void setDimension(int dimension) {
		this.dimension = dimension;
	}

	public double getOptimumFitness() {
		return optimumFitness;
	}

	public void setOptimumFitness(double optimumFitness) {
		this.optimumFitness = optimumFitness;
	}

	public double[] getOptimum() {
		return optimum;
	}

	public void setOptimum(double[] optimum) {
		this.optimum = optimum;
	}

	public Domain getDomain() {
		return domain;
	}

	public void setDomain(Domain domain) {
		this.domain = domain;
	}

	public ArrayList<Seed> getSample() {
		return sample;
	}

}
