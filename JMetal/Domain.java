/*
 * Domain.java
 * Copyright (c) 2016, Robert Gordon University, Aberdeen.
 * All rights reserved.
 */

package jmetal.problems.MO_ICOP;

import java.util.Arrays;

/**
 * Describes the domain for a multi-dimensional continuous optimisation problem.
 */
public class Domain {

	private final int dimension;
	private final double[] mins;
	private final double[] maxs;

	/**
	 * Constructs a new instance of a domain, which defaults to [0.0, 1.0]^dim.
	 *
	 * @param dim
	 *            the number of dimensions
	 */
	public Domain(int dim) {
		dimension = dim;
		mins = new double[dim];
		maxs = new double[dim];
		Arrays.fill(mins, 1.0);
	}

	/**
	 * Returns the minimum value for the specified dimension.
	 *
	 * @param i
	 *            the index, in the range [0, dim-1]
	 * @return the minimum value
	 */
	public double getMin(int i) {
		return mins[i];
	}

	/**
	 * Returns the maximum value for the specified dimension.
	 *
	 * @param i
	 *            the index, in the range [0, dim-1]
	 * @return the maximum value
	 */
	public double getMax(int i) {
		return maxs[i];
	}

	/**
	 * Sets the minimum value for the specified dimension.
	 *
	 * @param i
	 *            the index, in the range [0, dim-1]
	 * @param min
	 *            the minimum value
	 */
	public void setMin(int i, double min) {
		mins[i] = min;
	}

	/**
	 * Sets the maximum value for the specified dimension.
	 *
	 * @param i
	 *            the index, in the range [0, dim-1]
	 * @param max
	 *            the maximum value
	 */
	public void setMax(int i, double max) {
		maxs[i] = max;
	}

	/**
	 * Sets the values for the specified dimension.
	 *
	 * @param i
	 *            the index, in the range [0, dim-1]
	 * @param min
	 *            the minimum value
	 * @param max
	 *            the maximum value
	 */
	public void setValues(int i, double min, double max) {
		mins[i] = min;
		maxs[i] = max;
	}

	/**
	 * Sets the values for all dimensions.
	 *
	 * @param min
	 *            the minimum value
	 * @param max
	 *            the maximum value
	 */
	public void setValues(double min, double max) {
		for (int i = 0; i < dimension; i++) {
			mins[i] = min;
			maxs[i] = max;
		}
	}

	/**
	 * Returns the number of dimensions.
	 *
	 * @return the number of dimensions
	 */
	public int getDimension() {
		return dimension;
	}

	public double getDiagonal() {
		return Math.sqrt((double) dimension);
	}

}
