/*
 * STFProblem.java
 * Copyright (c) 2016, Robert Gordon University, Aberdeen.
 * All rights reserved.
 */

package jmetal.problems.MO_ICOP;

import java.util.ArrayList;

/**
 * Defines an STF problem instance.
 */
public class ICOProblem extends ProblemCont {

	private final ArrayList<Seed> seeds;
	private InterpolationMethod interpolationMethod;

	/**
	 * Constructs an instance of STF problem.
	 *
	 * @param dimension
	 *            the number of dimensions
	 * @param optimumFitness
	 *            the global optimum fitness value
	 * @param optimum
	 *            the global optimum candidate
	 * @param domain
	 *            the domain of the problem
	 * @param seeds
	 *            the seeds
	 */
	public ICOProblem(int dimension, double optimumFitness, double[] optimum, Domain domain, ArrayList<Seed> s,
			InterpolationMethod interpol) {
		super(dimension, optimumFitness, optimum, domain);

		interpolationMethod = interpol;
		seeds = s;

	}

	@Override
	public double evaluate(double[] sol) {

		long starttime = System.currentTimeMillis();
		double ret = interpolationMethod.interpolate(sol, seeds);
		// System.out.println("evaluation time = "+
		// (double)(System.currentTimeMillis()-starttime)/1000);
		return ret;
	}

	public ArrayList<Seed> getSeeds() {
		return seeds;
	}

	public Domain getDomain() {
		return domain;
	}

	public InterpolationMethod getInterpolationMethod() {
		return interpolationMethod;
	}

	public void setInterpolationMethod(InterpolationMethod interpolationMethod) {
		this.interpolationMethod = interpolationMethod;
	}

}
