/**
 * Kursawe.java
 *
 * @author Antonio J. Nebro
 * @author Juan J. Durillo
 * @version 2.0
 */
package jmetal.problems.MO_ICOP;

import java.io.IOException;

import jmetal.base.Problem;
import jmetal.base.Solution;
import jmetal.base.solutionType.ArrayRealSolutionType;
import jmetal.base.solutionType.BinaryRealSolutionType;
import jmetal.base.solutionType.RealSolutionType;
import jmetal.util.JMException;
import jmetal.util.wrapper.XReal;

/**
 * Class representing problem ICOP
 */
public class ICOP extends Problem {

	/**
	 * Stores the number of neighbours used in the interpolation
	 */
	protected int interpolationK_;

	/**
	 * Stores the 1syt ICOP problem
	 */
	protected ICOProblem pb1_;

	/**
	 * Stores the 2nd ICOP problem
	 */
	protected ICOProblem pb2_;

	/**
	 * Indicates if the individuals evaluated during the run are shown in the
	 * standard output
	 */
	protected boolean verboseRuntime_ = false;

	/**
	 * Constructor. Creates a default instance of the ICOP problem.
	 * 
	 * @param solutionType
	 *            The solution type must "Real", "BinaryReal, and "ArrayReal".
	 */
	public ICOP(String solutionType) throws ClassNotFoundException {
		this(solutionType, 1, 5, 1, false, 2);
	} // ICOP

	/**
	 * Constructor. Creates a new instance of the ICOP problem.
	 * 
	 * @param solutionType
	 *            The solution type must "Real", "BinaryReal, and "ArrayReal".
	 * @param problemID
	 *            Number of variables of the problem
	 */
	public ICOP(String solutionType, Integer problemID, Integer numberOfVariables, Integer interpolationK,
			boolean verboseRuntime, Integer numberOfObjectives) throws ClassNotFoundException {
		numberOfVariables_ = numberOfVariables;
		numberOfObjectives_ = numberOfObjectives;
		numberOfConstraints_ = 0;
		interpolationK_ = interpolationK;
		verboseRuntime_ = verboseRuntime;
		problemName_ = "" + problemID;

		upperLimit_ = new double[numberOfVariables_];
		lowerLimit_ = new double[numberOfVariables_];

		for (int i = 0; i < numberOfVariables_; i++) {
			lowerLimit_[i] = -5.0; // change as required
			upperLimit_[i] = 5.0; // change as required
		} // for

		if (solutionType.compareTo("BinaryReal") == 0)
			solutionType_ = new BinaryRealSolutionType(this);
		else if (solutionType.compareTo("Real") == 0)
			solutionType_ = new RealSolutionType(this);
		else if (solutionType.compareTo("ArrayReal") == 0)
			solutionType_ = new ArrayRealSolutionType(this);
		else {
			System.out.println("Error: solution type " + solutionType + " invalid");
			System.exit(-1);
		}

		Domain domain = new Domain(numberOfVariables_);
		domain.setValues(lowerLimit_[0], upperLimit_[0]);

		// Create interpolation method
		// interpolationK_ = 4; // change as required
		InterpolationMethod interpolationMethod = new IDWInterpolation(interpolationK_);

		// Create problem loader
		ProblemLoader loader = new ProblemLoader(domain, interpolationMethod);

		try {
			pb1_ = loader.loadProblem("jmetal/problems/MO_ICOP/sampledPoints/D" + numberOfVariables_ + "/MOOP"
					+ problemName_ + "/P1/");
			pb2_ = loader.loadProblem("jmetal/problems/MO_ICOP/sampledPoints/D" + numberOfVariables_ + "/MOOP"
					+ problemName_ + "/P2/");
		} catch (Exception e) {
			e.printStackTrace();
		}

	} // ICOP

	/**
	 * Evaluates a solution
	 * 
	 * @param solution
	 *            The solution to evaluate
	 * @throws JMException
	 * @throws IOException
	 */
	public void evaluate(Solution solution) throws JMException {
		double[] fx = new double[2]; // function values

		XReal x = new XReal(solution);
		double[] xD = new double[numberOfVariables_];

		String s = "";
		for (int i = 0; i < numberOfVariables_; i++) {
			xD[i] = x.getValue(i);
			s += xD[i] + ",";
		}
		fx[0] = pb1_.evaluate(xD);
		fx[1] = pb2_.evaluate(xD);
		s = s + fx[0] + "," + fx[1];
		if (verboseRuntime_) {
			System.out.println(s);
		}

		solution.setObjective(0, fx[0]);
		solution.setObjective(1, fx[1]);

	} // evaluate
} // ICOP
