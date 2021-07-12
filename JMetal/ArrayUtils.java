/*
 * ArrayUtils.java
 * Copyright (c) 2016, Robert Gordon University, Aberdeen.
 * All rights reserved.
 */

package jmetal.problems.MO_ICOP;

import java.text.DecimalFormat;
import java.util.NoSuchElementException;

/**
 * Utilities for manipulating arrays.
 */
public class ArrayUtils {

	/**
	 * Adds the elements of two arrays.
	 *
	 * @param a
	 *            the first array
	 * @param b
	 *            the second array
	 * @return the resulting array
	 */
	static public double[] sum(double[] a, double[] b) {

		double[] res = new double[a.length];

		for (int i = 0; i < a.length; i++) {
			res[i] = a[i] + b[i];
		}

		return res;
	}

	/**
	 * Adds the elements of two 2-d arrays.
	 *
	 * @param a
	 *            the first 2-d array
	 * @param b
	 *            the second 2-d array
	 * @return the resulting 2-d array
	 */
	static public double[][] sum(double[][] a, double[][] b) {

		double[][] res = new double[a.length][a[0].length];

		for (int i = 0; i < a.length; i++) {
			for (int j = 0; j < a[0].length; j++) {
				res[i][j] = a[i][j] + b[i][j];
			}
		}

		return res;
	}

	/**
	 * Subtracts the elements of two arrays.
	 *
	 * @param a
	 *            the first array
	 * @param b
	 *            the second array
	 * @return the resulting array
	 */
	static public double[] sub(double[] a, double[] b) {

		double[] res = new double[a.length];

		for (int i = 0; i < a.length; i++) {
			res[i] = a[i] - b[i];
		}

		return res;
	}

	/**
	 * Multiplies the elements of two arrays.
	 *
	 * @param a
	 *            the first array
	 * @param b
	 *            the second array
	 * @return the resulting array
	 */
	static public double[] mult(double[] a, double[] b) {

		double[] res = new double[a.length];

		for (int i = 0; i < a.length; i++) {
			res[i] = a[i] * b[i];
		}

		return res;
	}

	/**
	 * Divides the elements of two arrays.
	 *
	 * @param a
	 *            the first array
	 * @param b
	 *            the second array
	 * @return the resulting array
	 */
	static public double[] div(double[] a, double[] b) {

		double[] res = new double[a.length];

		for (int i = 0; i < a.length; i++) {
			res[i] = a[i] / b[i];
		}

		return res;
	}

	/**
	 * Multiplies the elements of an array by a scalar.
	 *
	 * @param a
	 *            the scalar
	 * @param b
	 *            the array
	 * @return the resulting array
	 */
	static public double[] mult(double a, double[] b) {

		double[] res = new double[b.length];

		for (int i = 0; i < b.length; i++) {
			res[i] = a * b[i];
		}

		return res;
	}

	/**
	 * Divides the elements of an array by a scalar.
	 *
	 * @param a
	 *            the scalar
	 * @param b
	 *            the array
	 * @return the resulting array
	 */
	static public double[] div(double a, double[] b) {

		double[] res = new double[b.length];

		for (int i = 0; i < b.length; i++) {
			res[i] = b[i] / a;
		}

		return res;
	}

	/**
	 * Divides the elements of a 2-d array by a scalar.
	 *
	 * @param a
	 *            the scalar
	 * @param b
	 *            the 2-d array
	 * @return the resulting 2-d array
	 */
	static public double[][] div(double a, double[][] b) {

		double[][] res = new double[b.length][b[0].length];
		for (int i = 0; i < b.length; i++) {
			for (int j = 0; j < b[0].length; j++) {
				res[i][j] = b[i][j] / a;
			}
		}

		return res;
	}

	/**
	 * Converts an array to a string.
	 *
	 * @param a
	 *            the array
	 * @return the string
	 */
	static public String print(double[] a) {
		String ret = "";
		for (int i = 0; i < a.length; i++) {
			ret += a[i] + "\t";

		}
		return ret;
	}

	/**
	 * Converts an array to a string.
	 *
	 * @param a
	 *            the array
	 * @return the string
	 */
	static public String print(int[] a) {
		String ret = "";
		for (int i = 0; i < a.length; i++) {
			ret += a[i] + "\t";

		}
		return ret;
	}

	/**
	 * Rounds the values in an array
	 *
	 * @param a
	 *            the array
	 */
	static public void round(double[] a) {
		for (int i = 0; i < a.length; i++) {
			a[i] = (double) Math.round(a[i]);

		}
	}

	/**
	 * Calculates the euclidian distance between two arrays.
	 *
	 * @param a
	 *            the first array
	 * @param b
	 *            the second array
	 * @return the distance
	 */
	static public double euclideanDistance(double[] a, double[] b) {
		double sum = 0;
		for (int i = 0; i < a.length; i++) {
			sum += Math.pow((a[i] - b[i]), 2.0);
		}
		return Math.sqrt(sum);
	}

	/**
	 * Converts an array for doubles to an array of formatted strings.
	 *
	 * @param a
	 *            the array
	 * @param format
	 *            the format
	 * @return the resulting array
	 */
	public static String[] doublesToStr(double[] a, String format) {
		DecimalFormat df = new DecimalFormat(format);
		String[] rv = new String[a.length];
		for (int i = 0; i < rv.length; i++) {
			rv[i] = df.format(a[i]);
		}
		return rv;
	}

	/**
	 * Returns the maximum element in an array.
	 *
	 * @param a
	 *            the array
	 * @return the maximum element
	 */
	public static double max(double[] a) {
		if (a.length == 0) {
			throw new NoSuchElementException();
		}
		double max = Double.NEGATIVE_INFINITY;
		for (double d : a) {
			if (Double.isNaN(d)) {
				throw new IllegalArgumentException("NaN");
			}
			if (d > max) {
				max = d;
			}
		}
		return max;
	}

	/**
	 * Returns the minimum element in an array.
	 *
	 * @param a
	 *            the array
	 * @return the minimum element
	 */
	public static double min(double[] a) {
		if (a.length == 0) {
			throw new NoSuchElementException();
		}
		double min = Double.POSITIVE_INFINITY;
		for (double d : a) {
			if (Double.isNaN(d)) {
				throw new IllegalArgumentException("NaN");
			}
			if (d < min) {
				min = d;
			}
		}
		return min;
	}

	/**
	 * Adds a scalar to the elements in an array.
	 *
	 * @param a
	 *            the scalar
	 * @param b
	 *            the array
	 * @return the resulting array
	 */
	public static double[] add(double a, double[] b) {
		double[] res = new double[b.length];
		for (int i = 0; i < b.length; i++) {
			res[i] = a + b[i];
		}
		return res;
	}

}
