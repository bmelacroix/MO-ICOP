package jmetal.problems.MO_ICOP;

import java.util.List;

public class IDWInterpolation extends InterpolationMethod {

	private double p;

	public IDWInterpolation(double p) {
		super();
		this.p = p;
	}

	@Override
	public double interpolate(double[] genes, List<Seed> seeds) {

		double[] weights = new double[seeds.size()];
		double sumOfWeights = 0;
		double ret = 0;

		for (int i = 0; i < seeds.size(); i++) {
			double dist = ArrayUtils.euclideanDistance(genes, seeds.get(i).getGenes());
			if (dist == 0) {
				return seeds.get(i).getFitness();
			}
			weights[i] = 1 / Math.pow(dist, p);
			sumOfWeights += weights[i];
		}

		for (int i = 0; i < seeds.size(); i++) {
			ret += weights[i] * seeds.get(i).getFitness() / sumOfWeights;
		}

		return ret;
	}

	public double getP() {
		return p;
	}

	public void setP(double p) {
		this.p = p;
	}

}
