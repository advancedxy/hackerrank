//MaHDi MoHAJeri - Islamic Republic of Iran
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class P100 {
	static final int PQ_LIMIT = 10000000;
	static final BigInteger LIMIT = new BigInteger(Long.MAX_VALUE + "");

	public static void main(String[] args) {
		System.err.println(solve(1, 2, 1000000000000L).blue);

		Scanner in = new Scanner(System.in);
		int T = in.nextInt();
		for (int tc = 0; tc < T; tc++) {
			int P = in.nextInt();
			int Q = in.nextInt();
			long D = in.nextLong();

			BlueAndTotal result = solve(P, Q, D);
			System.out.println(result == null ? "No solution" : result.blue
					+ " " + result.total);
		}
		in.close();
	}

	static BlueAndTotal solve(int P, int Q, long D) {
		if (isSquare((long) P * Q)) {
			return solvePQSquareCase(P, Q, D);
		}

		BigInteger biP = new BigInteger(P + "");
		BigInteger biQ = new BigInteger(Q + "");
		BigInteger biD = new BigInteger(D + "");

		BlueAndTotal result = null;

		for (int diff : findDivisors(Q - P)) {
			BigInteger leftNumerator = BigInteger.ZERO;
			BigInteger leftDenominator = new BigInteger(diff + "");
			BigInteger rightNumerator = BigInteger.ONE;
			BigInteger rightDenominator = BigInteger.ONE;

			BlueAndTotal blueAndTotal = null;
			while (leftDenominator.compareTo(LIMIT) <= 0
					&& rightDenominator.compareTo(LIMIT) <= 0) {

				BigInteger nextNumerator = leftNumerator.add(rightNumerator);
				BigInteger nextDenominator = leftDenominator
						.add(rightDenominator);

				boolean less = isLess(nextNumerator, nextDenominator, biP, biQ);

				if (less) {
					// Speed up left update
					BigInteger leftStep = computeLeftStep(biP, biQ,
							leftNumerator, leftDenominator, rightNumerator,
							rightDenominator);

					nextNumerator = leftNumerator.add(rightNumerator
							.multiply(leftStep));
					nextDenominator = leftDenominator.add(rightDenominator
							.multiply(leftStep));
				}

				if (less) {
					leftNumerator = nextNumerator;
					leftDenominator = nextDenominator;
				} else {
					rightNumerator = nextNumerator;
					rightDenominator = nextDenominator;
				}

				BigInteger pairedNumerator = biP.multiply(nextDenominator);
				BigInteger pairedDenominator = biQ.multiply(nextNumerator);
				BigInteger common = gcd(pairedNumerator, pairedDenominator);
				pairedNumerator = pairedNumerator.divide(common);
				pairedDenominator = pairedDenominator.divide(common);

				blueAndTotal = less ? findBlueAndTotal(nextNumerator,
						nextDenominator, pairedNumerator, pairedDenominator)
						: findBlueAndTotal(pairedNumerator, pairedDenominator,
								nextNumerator, nextDenominator);
				if (blueAndTotal != null
						&& blueAndTotal.total.compareTo(biD) > 0) {
					break;
				}
			}
			if (blueAndTotal != null
					&& (result == null || blueAndTotal.total
							.compareTo(result.total) < 0)) {
				result = blueAndTotal;
			}
		}
		return result;
	}

	static BigInteger computeLeftStep(BigInteger p, BigInteger q,
			BigInteger leftNumerator, BigInteger leftDenominator,
			BigInteger rightNumerator, BigInteger rightDenominator) {
		int lower = 1;
		int upper = PQ_LIMIT;
		int leftStep = 1;
		while (lower <= upper) {
			BigInteger middle = new BigInteger((lower + upper) / 2 + "");

			BigInteger nextNumerator = leftNumerator.add(rightNumerator
					.multiply(middle));
			BigInteger nextDenominator = leftDenominator.add(rightDenominator
					.multiply(middle));

			if (isLess(nextNumerator, nextDenominator, p, q)) {
				leftStep = Math.max(leftStep, middle.intValue());

				lower = middle.intValue() + 1;
			} else {
				upper = middle.intValue() - 1;
			}
		}

		return new BigInteger(leftStep + "");
	}

	static List<Integer> findDivisors(int number) {
		List<PrimeFactorAndCount> primeFactorAndCounts = findPrimeFactorAndCounts(number);
		List<Integer> divisors = new ArrayList<Integer>();
		search(divisors, primeFactorAndCounts, 0, 1);
		return divisors;
	}

	static void search(List<Integer> divisors,
			List<PrimeFactorAndCount> primeFactorAndCounts, int index,
			int divisor) {
		if (index == primeFactorAndCounts.size()) {
			divisors.add(divisor);
			return;
		}

		PrimeFactorAndCount primeFactorAndCount = primeFactorAndCounts
				.get(index);
		search(divisors, primeFactorAndCounts, index + 1, divisor);
		for (int i = 0; i < primeFactorAndCount.count; i++) {
			divisor *= primeFactorAndCount.primeFactor;
			search(divisors, primeFactorAndCounts, index + 1, divisor);
		}
	}

	static List<PrimeFactorAndCount> findPrimeFactorAndCounts(int number) {
		List<PrimeFactorAndCount> primeFactors = new ArrayList<PrimeFactorAndCount>();
		for (int primeFactor = 2; primeFactor * primeFactor <= number; primeFactor++) {
			if (isPrime(primeFactor) && number % primeFactor == 0) {
				int count = 0;
				while (number % primeFactor == 0) {
					count++;
					number /= primeFactor;
				}
				primeFactors.add(new PrimeFactorAndCount(primeFactor, count));
			}
		}
		if (number > 1) {
			primeFactors.add(new PrimeFactorAndCount(number, 1));
		}
		return primeFactors;
	}

	static boolean isPrime(int number) {
		for (int i = 2; i * i <= number; i++) {
			if (number % i == 0) {
				return false;
			}
		}
		return true;
	}

	static BlueAndTotal findBlueAndTotal(BigInteger leftNumerator,
			BigInteger leftDenominator, BigInteger rightNumerator,
			BigInteger rightDenominator) {
		BigInteger d = leftDenominator.multiply(rightNumerator).subtract(
				leftNumerator.multiply(rightDenominator));
		if (leftNumerator.multiply(rightDenominator.subtract(rightNumerator))
				.mod(d).signum() != 0
				|| leftDenominator
						.multiply(rightDenominator.subtract(rightNumerator))
						.mod(d).signum() != 0
				|| rightNumerator
						.multiply(leftDenominator.subtract(leftNumerator))
						.mod(d).signum() != 0
				|| rightDenominator
						.multiply(leftDenominator.subtract(leftNumerator))
						.mod(d).signum() != 0) {
			return null;
		}
		return new BlueAndTotal(rightNumerator.multiply(
				leftDenominator.subtract(leftNumerator)).divide(d),
				rightDenominator.multiply(
						leftDenominator.subtract(leftNumerator)).divide(d));
	}

	static BigInteger gcd(BigInteger a, BigInteger b) {
		if (b.signum() == 0) {
			return a;
		}
		return gcd(b, a.mod(b));
	}

	static boolean isSquare(long number) {
		long root = Math.round(Math.sqrt(number));
		return root * root == number;
	}

	static boolean isLess(BigInteger numerator, BigInteger denominator,
			BigInteger p, BigInteger q) {
		return numerator.multiply(numerator).multiply(q)
				.compareTo(denominator.multiply(denominator).multiply(p)) < 0;
	}

	static BlueAndTotal solvePQSquareCase(int P, int Q, long D) {
		long W = Math.round(Math.sqrt((long) P * Q));
		long product = (long) Q * (Q - P);

		BlueAndTotal result = null;
		for (long smallFactor = 1; smallFactor * smallFactor < product; smallFactor++) {
			if (product % smallFactor == 0) {
				long bigFactor = product / smallFactor;

				long Q2s = bigFactor + smallFactor;
				if (Q2s % (Q * 2) != 0) {
					continue;
				}
				long s = Q2s / (Q * 2);

				long W2t = bigFactor - smallFactor;
				if (W2t % (W * 2) != 0) {
					continue;
				}
				long t = W2t / (W * 2);

				if (s % 2 == 0 || t % 2 == 0) {
					continue;
				}
				long x = (s + 1) / 2;
				long y = (t + 1) / 2;

				if (y > D
						&& (result == null || y < result.total.longValue())) {
					result = new BlueAndTotal(new BigInteger(x + ""),
							new BigInteger(y + ""));
				}
			}
		}
		return result;
	}
}

class PrimeFactorAndCount {
	int primeFactor;
	int count;

	PrimeFactorAndCount(int primeFactor, int count) {
		this.primeFactor = primeFactor;
		this.count = count;
	}
}

class BlueAndTotal {
	BigInteger blue;
	BigInteger total;

	BlueAndTotal(BigInteger blue, BigInteger total) {
		this.blue = blue;
		this.total = total;
	}
}
