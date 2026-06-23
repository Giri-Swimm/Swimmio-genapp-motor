import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.*;
import java.time.LocalDate;

/**
 * LGAPDB04 - Advanced Actuarial Premium Calculations
 *
 * This program calculates insurance premiums based on various actuarial factors,
 * risk assessments, and coverage selections for commercial property insurance.
 */
public class LGAPDB04 {

    // Actuarial Constants
    private static final BigDecimal EXPENSE_RATIO = new BigDecimal("0.350");
    private static final BigDecimal PROFIT_MARGIN = new BigDecimal("0.150");
    private static final BigDecimal BASE_LOSS_RATIO = new BigDecimal("0.600");
    private static final BigDecimal TREND_FACTOR = new BigDecimal("1.0350");
    private static final BigDecimal CREDIBILITY_FACTOR = new BigDecimal("0.750");

    // Peril Correlations
    private static final BigDecimal FIRE_WEATHER_CORR = new BigDecimal("0.250");
    private static final BigDecimal FLOOD_WEATHER_CORR = new BigDecimal("0.850");
    private static final BigDecimal CRIME_LOCATION_CORR = new BigDecimal("0.450");

    // Catastrophe Model Factors
    private static final BigDecimal HURRICANE_FACTOR = new BigDecimal("0.0125");
    private static final BigDecimal EARTHQUAKE_FACTOR = new BigDecimal("0.0080");
    private static final BigDecimal TORNADO_FACTOR = new BigDecimal("0.0045");
    private static final BigDecimal FLOOD_FACTOR = new BigDecimal("0.0090");

    // Tax rate
    private static final BigDecimal TAX_RATE = new BigDecimal("0.0675");

    // Database connection
    private Connection connection;

    /**
     * Input data structure
     */
    public static class InputData {
        public String customerNum;
        public int riskScore;
        public String propertyType;
        public String territory;
        public String constructionType;
        public String occupancyCode;
        public String protectionClass;
        public int yearBuilt;
        public int squareFootage;
        public int yearsInBusiness;
        public int claimsCount5Yr;
        public BigDecimal claimsAmount5Yr;
    }

    /**
     * Coverage data structure
     */
    public static class CoverageData {
        public BigDecimal buildingLimit;
        public BigDecimal contentsLimit;
        public BigDecimal biLimit;
        public BigDecimal fireDeductible;
        public BigDecimal windDeductible;
        public BigDecimal floodDeductible;
        public BigDecimal otherDeductible;
        public int firePeril;
        public int crimePeril;
        public int floodPeril;
        public int weatherPeril;
    }

    /**
     * Output results structure
     */
    public static class OutputResults {
        // Calculated premiums by peril
        public BigDecimal firePremium = BigDecimal.ZERO;
        public BigDecimal crimePremium = BigDecimal.ZERO;
        public BigDecimal floodPremium = BigDecimal.ZERO;
        public BigDecimal weatherPremium = BigDecimal.ZERO;
        public BigDecimal totalPremium = BigDecimal.ZERO;

        // Premium components
        public BigDecimal baseAmount = BigDecimal.ZERO;
        public BigDecimal catLoadAmt = BigDecimal.ZERO;
        public BigDecimal expenseLoadAmt = BigDecimal.ZERO;
        public BigDecimal profitLoadAmt = BigDecimal.ZERO;
        public BigDecimal discountAmt = BigDecimal.ZERO;
        public BigDecimal taxAmt = BigDecimal.ZERO;

        // Rating factors
        public BigDecimal experienceMod = BigDecimal.ONE;
        public BigDecimal scheduleMod = BigDecimal.ZERO;
        public BigDecimal finalRateFactor = BigDecimal.ZERO;
    }

    /**
     * Calculation work areas
     */
    private static class CalculationAreas {
        // Exposure calculations
        BigDecimal buildingExposure = BigDecimal.ZERO;
        BigDecimal contentsExposure = BigDecimal.ZERO;
        BigDecimal biExposure = BigDecimal.ZERO;
        BigDecimal totalInsuredVal = BigDecimal.ZERO;
        BigDecimal exposureDensity = BigDecimal.ZERO;

        // Base rates by peril [fire, crime, flood, weather]
        BigDecimal[] baseRates = new BigDecimal[4];

        // Discount calculations
        BigDecimal multiPerilDisc = BigDecimal.ZERO;
        BigDecimal claimsFreeDisc = BigDecimal.ZERO;
        BigDecimal deductibleCredit = BigDecimal.ZERO;
        BigDecimal totalDiscount = BigDecimal.ZERO;

        // Intermediate results
        BigDecimal catLoading = BigDecimal.ZERO;
        BigDecimal expenseLoading = BigDecimal.ZERO;
        BigDecimal profitLoading = BigDecimal.ZERO;
    }

    public LGAPDB04(Connection connection) {
        this.connection = connection;
    }

    /**
     * Main calculation entry point
     */
    public void calculatePremium(InputData inputData, CoverageData coverageData,
                                 OutputResults outputResults) throws SQLException {
        CalculationAreas calc = new CalculationAreas();

        // Initialize calculations
        p200Init(inputData, coverageData, calc);

        // Load base rates
        p300LoadRates(inputData, calc);

        // Calculate experience modifier
        p400ExperienceMod(inputData, calc, outputResults);

        // Calculate schedule modifier
        p500ScheduleMod(inputData, calc, outputResults);

        // Calculate base premiums
        p600BasePremium(inputData, coverageData, calc, outputResults);

        // Calculate catastrophe loading
        p700CatLoading(coverageData, calc, outputResults);

        // Calculate expense and profit loading
        p800ExpenseAndProfit(calc, outputResults);

        // Calculate discounts
        p900Discounts(inputData, coverageData, calc, outputResults);

        // Calculate taxes
        p950Taxes(outputResults);

        // Final calculations
        p999Final(calc, outputResults);
    }

    /**
     * Initialize exposure calculations
     */
    private void p200Init(InputData input, CoverageData coverage, CalculationAreas calc) {
        // Calculate risk adjustment factor
        BigDecimal riskAdjustment = new BigDecimal(input.riskScore - 100)
            .divide(new BigDecimal("1000"), 6, RoundingMode.HALF_UP)
            .add(BigDecimal.ONE);

        // Calculate exposures with risk adjustment
        calc.buildingExposure = coverage.buildingLimit.multiply(riskAdjustment);
        calc.contentsExposure = coverage.contentsLimit.multiply(riskAdjustment);
        calc.biExposure = coverage.biLimit.multiply(riskAdjustment);

        // Calculate total insured value
        calc.totalInsuredVal = calc.buildingExposure
            .add(calc.contentsExposure)
            .add(calc.biExposure);

        // Calculate exposure density (per square foot)
        if (input.squareFootage > 0) {
            calc.exposureDensity = calc.totalInsuredVal
                .divide(new BigDecimal(input.squareFootage), 4, RoundingMode.HALF_UP);
        } else {
            calc.exposureDensity = new BigDecimal("100.00");
        }

        // Initialize base rates array
        for (int i = 0; i < 4; i++) {
            calc.baseRates[i] = BigDecimal.ZERO;
        }
    }

    /**
     * Load base rates from database
     */
    private void p300LoadRates(InputData input, CalculationAreas calc) throws SQLException {
        String[] perilCodes = {"FI", "CR", "FL", "WE"};
        BigDecimal[] defaultRates = {
            new BigDecimal("0.008500"),
            new BigDecimal("0.006200"),
            new BigDecimal("0.012800"),
            new BigDecimal("0.009600")
        };

        String sql = "SELECT BASE_RATE, MIN_PREMIUM, MAX_PREMIUM " +
                     "FROM RATE_MASTER " +
                     "WHERE TERRITORY = ? " +
                     "AND CONSTRUCTION_TYPE = ? " +
                     "AND OCCUPANCY_CODE = ? " +
                     "AND PERIL_CODE = ? " +
                     "AND EFFECTIVE_DATE <= ? " +
                     "AND EXPIRY_DATE >= ?";

        LocalDate currentDate = LocalDate.now();

        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            for (int i = 0; i < perilCodes.length; i++) {
                stmt.setString(1, input.territory);
                stmt.setString(2, input.constructionType);
                stmt.setString(3, input.occupancyCode);
                stmt.setString(4, perilCodes[i]);
                stmt.setDate(5, Date.valueOf(currentDate));
                stmt.setDate(6, Date.valueOf(currentDate));

                try (ResultSet rs = stmt.executeQuery()) {
                    if (rs.next()) {
                        calc.baseRates[i] = rs.getBigDecimal("BASE_RATE");
                    } else {
                        calc.baseRates[i] = defaultRates[i];
                    }
                }
            }
        }
    }

    /**
     * Calculate experience modification factor
     */
    private void p400ExperienceMod(InputData input, CalculationAreas calc,
                                   OutputResults output) {
        BigDecimal expMod = BigDecimal.ONE;

        if (input.yearsInBusiness >= 5) {
            if (input.claimsCount5Yr == 0) {
                expMod = new BigDecimal("0.8500");
            } else {
                // Calculate loss ratio impact
                BigDecimal lossRatio = input.claimsAmount5Yr
                    .divide(calc.totalInsuredVal, 6, RoundingMode.HALF_UP);

                BigDecimal adjustment = lossRatio
                    .multiply(CREDIBILITY_FACTOR)
                    .multiply(new BigDecimal("0.50"));

                expMod = BigDecimal.ONE.add(adjustment);

                // Cap between 0.5 and 2.0
                if (expMod.compareTo(new BigDecimal("2.0000")) > 0) {
                    expMod = new BigDecimal("2.0000");
                }
                if (expMod.compareTo(new BigDecimal("0.5000")) < 0) {
                    expMod = new BigDecimal("0.5000");
                }
            }
        } else {
            expMod = new BigDecimal("1.1000");
        }

        output.experienceMod = expMod;
    }

    /**
     * Calculate schedule modification factor
     */
    private void p500ScheduleMod(InputData input, CalculationAreas calc,
                                 OutputResults output) {
        BigDecimal schedMod = BigDecimal.ZERO;

        // Building age factor
        if (input.yearBuilt >= 2010) {
            schedMod = schedMod.subtract(new BigDecimal("0.050"));
        } else if (input.yearBuilt >= 1990) {
            // No change
        } else if (input.yearBuilt >= 1970) {
            schedMod = schedMod.add(new BigDecimal("0.100"));
        } else {
            schedMod = schedMod.add(new BigDecimal("0.200"));
        }

        // Protection class factor
        int protectionClass = Integer.parseInt(input.protectionClass);
        if (protectionClass >= 1 && protectionClass <= 3) {
            schedMod = schedMod.subtract(new BigDecimal("0.100"));
        } else if (protectionClass >= 4 && protectionClass <= 6) {
            schedMod = schedMod.subtract(new BigDecimal("0.050"));
        } else if (protectionClass >= 7 && protectionClass <= 9) {
            // No change
        } else {
            schedMod = schedMod.add(new BigDecimal("0.150"));
        }

        // Occupancy hazard factor
        if (input.occupancyCode.startsWith("OFF")) {
            schedMod = schedMod.subtract(new BigDecimal("0.025"));
        } else if (input.occupancyCode.startsWith("MFG")) {
            schedMod = schedMod.add(new BigDecimal("0.075"));
        } else if (input.occupancyCode.startsWith("WHS")) {
            schedMod = schedMod.add(new BigDecimal("0.125"));
        }

        // Exposure density factor
        if (calc.exposureDensity.compareTo(new BigDecimal("500.00")) > 0) {
            schedMod = schedMod.add(new BigDecimal("0.100"));
        } else if (calc.exposureDensity.compareTo(new BigDecimal("50.00")) < 0) {
            schedMod = schedMod.subtract(new BigDecimal("0.050"));
        }

        // Cap schedule mod between -0.200 and +0.400
        if (schedMod.compareTo(new BigDecimal("0.400")) > 0) {
            schedMod = new BigDecimal("0.400");
        }
        if (schedMod.compareTo(new BigDecimal("-0.200")) < 0) {
            schedMod = new BigDecimal("-0.200");
        }

        output.scheduleMod = schedMod;
    }

    /**
     * Calculate base premiums for each peril
     */
    private void p600BasePremium(InputData input, CoverageData coverage,
                                 CalculationAreas calc, OutputResults output) {
        BigDecimal scheduleModifier = BigDecimal.ONE.add(output.scheduleMod);

        // FIRE PREMIUM
        if (coverage.firePeril > 0) {
            BigDecimal fireExposure = calc.buildingExposure.add(calc.contentsExposure);
            output.firePremium = fireExposure
                .multiply(calc.baseRates[0])
                .multiply(output.experienceMod)
                .multiply(scheduleModifier)
                .multiply(TREND_FACTOR)
                .setScale(2, RoundingMode.HALF_UP);

            output.baseAmount = output.baseAmount.add(output.firePremium);
        }

        // CRIME PREMIUM
        if (coverage.crimePeril > 0) {
            BigDecimal crimeExposure = calc.contentsExposure
                .multiply(new BigDecimal("0.80"));
            output.crimePremium = crimeExposure
                .multiply(calc.baseRates[1])
                .multiply(output.experienceMod)
                .multiply(scheduleModifier)
                .multiply(TREND_FACTOR)
                .setScale(2, RoundingMode.HALF_UP);

            output.baseAmount = output.baseAmount.add(output.crimePremium);
        }

        // FLOOD PREMIUM
        if (coverage.floodPeril > 0) {
            output.floodPremium = calc.buildingExposure
                .multiply(calc.baseRates[2])
                .multiply(output.experienceMod)
                .multiply(scheduleModifier)
                .multiply(TREND_FACTOR)
                .multiply(new BigDecimal("1.25"))
                .setScale(2, RoundingMode.HALF_UP);

            output.baseAmount = output.baseAmount.add(output.floodPremium);
        }

        // WEATHER PREMIUM
        if (coverage.weatherPeril > 0) {
            BigDecimal weatherExposure = calc.buildingExposure.add(calc.contentsExposure);
            output.weatherPremium = weatherExposure
                .multiply(calc.baseRates[3])
                .multiply(output.experienceMod)
                .multiply(scheduleModifier)
                .multiply(TREND_FACTOR)
                .setScale(2, RoundingMode.HALF_UP);

            output.baseAmount = output.baseAmount.add(output.weatherPremium);
        }
    }

    /**
     * Calculate catastrophe loading
     */
    private void p700CatLoading(CoverageData coverage, CalculationAreas calc,
                                OutputResults output) {
        calc.catLoading = BigDecimal.ZERO;

        // Hurricane loading (wind/weather peril)
        if (coverage.weatherPeril > 0) {
            calc.catLoading = calc.catLoading.add(
                output.weatherPremium.multiply(HURRICANE_FACTOR)
            );
        }

        // Earthquake loading (affects all perils)
        calc.catLoading = calc.catLoading.add(
            output.baseAmount.multiply(EARTHQUAKE_FACTOR)
        );

        // Tornado loading (weather peril primarily)
        if (coverage.weatherPeril > 0) {
            calc.catLoading = calc.catLoading.add(
                output.weatherPremium.multiply(TORNADO_FACTOR)
            );
        }

        // Flood catastrophe loading
        if (coverage.floodPeril > 0) {
            calc.catLoading = calc.catLoading.add(
                output.floodPremium.multiply(FLOOD_FACTOR)
            );
        }

        output.catLoadAmt = calc.catLoading.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Calculate expense and profit loading
     */
    private void p800ExpenseAndProfit(CalculationAreas calc, OutputResults output) {
        // Expense loading
        calc.expenseLoading = output.baseAmount
            .add(output.catLoadAmt)
            .multiply(EXPENSE_RATIO);
        output.expenseLoadAmt = calc.expenseLoading.setScale(2, RoundingMode.HALF_UP);

        // Profit loading
        calc.profitLoading = output.baseAmount
            .add(output.catLoadAmt)
            .add(calc.expenseLoading)
            .multiply(PROFIT_MARGIN);
        output.profitLoadAmt = calc.profitLoading.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Calculate discounts
     */
    private void p900Discounts(InputData input, CoverageData coverage,
                              CalculationAreas calc, OutputResults output) {
        calc.totalDiscount = BigDecimal.ZERO;

        // Multi-peril discount
        calc.multiPerilDisc = BigDecimal.ZERO;
        if (coverage.firePeril > 0 && coverage.crimePeril > 0 &&
            coverage.floodPeril > 0 && coverage.weatherPeril > 0) {
            calc.multiPerilDisc = new BigDecimal("0.100");
        } else if (coverage.firePeril > 0 && coverage.weatherPeril > 0 &&
                   (coverage.crimePeril > 0 || coverage.floodPeril > 0)) {
            calc.multiPerilDisc = new BigDecimal("0.050");
        }

        // Claims-free discount
        calc.claimsFreeDisc = BigDecimal.ZERO;
        if (input.claimsCount5Yr == 0 && input.yearsInBusiness >= 5) {
            calc.claimsFreeDisc = new BigDecimal("0.075");
        }

        // Deductible credit
        calc.deductibleCredit = BigDecimal.ZERO;
        if (coverage.fireDeductible.compareTo(new BigDecimal("10000")) >= 0) {
            calc.deductibleCredit = calc.deductibleCredit
                .add(new BigDecimal("0.025"));
        }
        if (coverage.windDeductible.compareTo(new BigDecimal("25000")) >= 0) {
            calc.deductibleCredit = calc.deductibleCredit
                .add(new BigDecimal("0.035"));
        }
        if (coverage.floodDeductible.compareTo(new BigDecimal("50000")) >= 0) {
            calc.deductibleCredit = calc.deductibleCredit
                .add(new BigDecimal("0.045"));
        }

        // Total discount
        calc.totalDiscount = calc.multiPerilDisc
            .add(calc.claimsFreeDisc)
            .add(calc.deductibleCredit);

        // Cap at 25%
        if (calc.totalDiscount.compareTo(new BigDecimal("0.250")) > 0) {
            calc.totalDiscount = new BigDecimal("0.250");
        }

        // Calculate discount amount
        output.discountAmt = output.baseAmount
            .add(output.catLoadAmt)
            .add(output.expenseLoadAmt)
            .add(output.profitLoadAmt)
            .multiply(calc.totalDiscount)
            .setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Calculate taxes
     */
    private void p950Taxes(OutputResults output) {
        output.taxAmt = output.baseAmount
            .add(output.catLoadAmt)
            .add(output.expenseLoadAmt)
            .add(output.profitLoadAmt)
            .subtract(output.discountAmt)
            .multiply(TAX_RATE)
            .setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Final premium calculation
     */
    private void p999Final(CalculationAreas calc, OutputResults output) {
        // Calculate total premium
        output.totalPremium = output.baseAmount
            .add(output.catLoadAmt)
            .add(output.expenseLoadAmt)
            .add(output.profitLoadAmt)
            .subtract(output.discountAmt)
            .add(output.taxAmt)
            .setScale(2, RoundingMode.HALF_UP);

        // Calculate final rate factor
        if (calc.totalInsuredVal.compareTo(BigDecimal.ZERO) > 0) {
            output.finalRateFactor = output.totalPremium
                .divide(calc.totalInsuredVal, 6, RoundingMode.HALF_UP);

            // Cap rate factor at 5%
            if (output.finalRateFactor.compareTo(new BigDecimal("0.050000")) > 0) {
                output.finalRateFactor = new BigDecimal("0.050000");
                output.totalPremium = calc.totalInsuredVal
                    .multiply(output.finalRateFactor)
                    .setScale(2, RoundingMode.HALF_UP);
            }
        }
    }

    /**
     * Example usage
     */
    public static void main(String[] args) {
        try {
            // Setup database connection (adjust connection parameters as needed)
            Connection conn = DriverManager.getConnection(
                "jdbc:db2://localhost:50000/INSURANCE", "user", "password");

            LGAPDB04 calculator = new LGAPDB04(conn);

            // Create sample input data
            InputData input = new InputData();
            input.customerNum = "CUST123456";
            input.riskScore = 105;
            input.propertyType = "Commercial";
            input.territory = "TX001";
            input.constructionType = "CON";
            input.occupancyCode = "OFF01";
            input.protectionClass = "03";
            input.yearBuilt = 2015;
            input.squareFootage = 50000;
            input.yearsInBusiness = 10;
            input.claimsCount5Yr = 1;
            input.claimsAmount5Yr = new BigDecimal("25000.00");

            // Create sample coverage data
            CoverageData coverage = new CoverageData();
            coverage.buildingLimit = new BigDecimal("2000000.00");
            coverage.contentsLimit = new BigDecimal("500000.00");
            coverage.biLimit = new BigDecimal("1000000.00");
            coverage.fireDeductible = new BigDecimal("10000.00");
            coverage.windDeductible = new BigDecimal("25000.00");
            coverage.floodDeductible = new BigDecimal("50000.00");
            coverage.otherDeductible = new BigDecimal("5000.00");
            coverage.firePeril = 1;
            coverage.crimePeril = 1;
            coverage.floodPeril = 1;
            coverage.weatherPeril = 1;

            // Calculate premium
            OutputResults results = new OutputResults();
            calculator.calculatePremium(input, coverage, results);

            // Print results
            System.out.println("=== Premium Calculation Results ===");
            System.out.println("Fire Premium: $" + results.firePremium);
            System.out.println("Crime Premium: $" + results.crimePremium);
            System.out.println("Flood Premium: $" + results.floodPremium);
            System.out.println("Weather Premium: $" + results.weatherPremium);
            System.out.println("\n--- Premium Components ---");
            System.out.println("Base Amount: $" + results.baseAmount);
            System.out.println("Catastrophe Loading: $" + results.catLoadAmt);
            System.out.println("Expense Loading: $" + results.expenseLoadAmt);
            System.out.println("Profit Loading: $" + results.profitLoadAmt);
            System.out.println("Discount Amount: -$" + results.discountAmt);
            System.out.println("Tax Amount: $" + results.taxAmt);
            System.out.println("\n--- Rating Factors ---");
            System.out.println("Experience Modifier: " + results.experienceMod);
            System.out.println("Schedule Modifier: " + results.scheduleMod);
            System.out.println("Final Rate Factor: " + results.finalRateFactor);
            System.out.println("\n=== TOTAL PREMIUM: $" + results.totalPremium + " ===");

            conn.close();

        } catch (SQLException e) {
            System.err.println("Database error: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
