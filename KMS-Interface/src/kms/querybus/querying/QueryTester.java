package kms.querybus.querying;

public class QueryTester {
	public static void main(String[] args) {
		QueryManager queryManager = new QueryManager();
		System.out.println("exists? " + queryManager.checkProcedureExistence("auctionfees", "calculateFee", 2));
	}
}
