package jmetal.problems.MO_ICOP;

import java.util.List;

public abstract class InterpolationMethod {

	abstract public double interpolate(double[] genes, List<Seed> seeds);
}
