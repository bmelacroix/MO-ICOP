package jmetal.problems.MO_ICOP;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

public class ProblemLoader {

	private Domain domain;
	private InterpolationMethod interpolationMethod;

	public ProblemLoader(Domain domain, InterpolationMethod interpolationMethod) {
		super();
		this.domain = domain;
		this.interpolationMethod = interpolationMethod;
	}

	public ProblemLoader() {
		// TODO Auto-generated constructor stub
	}

	public ICOProblem loadProblem(String path) throws IOException {

		int dimension = 0;
		double optimumFitness = Double.MAX_VALUE;
		Seed bestSeed = null;

		ArrayList<Seed> seeds = new ArrayList<Seed>();

		BufferedReader csvReader = new BufferedReader(new FileReader(path + "Xs.csv"));
		String row;
		while ((row = csvReader.readLine()) != null) {
			String[] data = row.split(",");
			double[] var = new double[data.length];
			for (int i = 0; i < data.length; i++) {
				var[i] = Double.parseDouble(data[i]);
			}
			seeds.add(new Seed(var));
		}
		csvReader.close();

		csvReader = new BufferedReader(new FileReader(path + "Ys.csv"));

		for (Seed seed : seeds) {
			double fitness = Double.parseDouble(csvReader.readLine());
			seed.setFitness(fitness);
			if (fitness < optimumFitness) {
				bestSeed = seed;
			}
			// System.out.println(seed);
		}

		ICOProblem problem = new ICOProblem(dimension, bestSeed.getFitness(), bestSeed.getGenes(), domain, seeds,
				interpolationMethod);

		return problem;

	}

}
