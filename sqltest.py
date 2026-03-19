from pyspark.sql import SparkSession

spark = SparkSession.builder.getOrCreate()

namespace = "database_j5y8vfv2q32gpgxb6jkzyk3v__study_ofhs240215_20260130103024"
spark.sql(f"USE {namespace}")
spark.sql("SHOW TABLES").select("tableName").show(truncate=False)
tables = [t.name for t in spark.catalog.listTables(namespace)]
tables
spark.sql("SHOW TABLES IN database_j5y8vfv2q32gpgxb6jkzyk3v__study_ofhs240215_20260130103024").count()

questionnaire = spark.sql("""
SELECT 
  PID,
  DIAG_RESP_1_M,
  WORK_NIGHTS_1_1,
  WORK_SHIFTS_1_1,
  DIAG_2_M,
  SUBMISSION_DATE,
  MEDICAT_1_M,
  MEDICAT_A_1_M,
  MEDICAT_REPRO_1_M,
  MEDICAT_REPRO_CONTRACEPT_1_M,
  MEDICAT_RESP_1_M,
  GYN_CONTRACEPT_PILL_1_1,
  GYN_CONTRACEPT_PILL_FIRST_AGE_1_1,
  GYN_CONTRACEPT_PILL_LAST_AGE_1_1,
  GYN_CONTRACEPT_METHODS_1_M,
  ALCOHOL_CURR_1_1,
  ALCOHOL_PREV_1_1,
  ACTIVITY_WALK_DAYS_2_1,
  ACTIVITY_WALK_MINS_2_1,
  ACTIVITY_MOD_DAYS_2_1,
  ACTIVITY_MOD_MINS_2_1,
  ACTIVITY_VIG_DAYS_2_1,
  ACTIVITY_VIG_MINS_2_1,
  WORK_WK_HRS_1_1,
  SLEEP_CHRONOTYPE_1_1,
  SLEEP_HRS_1_1,
  MEDICAT_PSYCH_1_M,
  HOUSING_INCOME_1_1,
  GYN_HRT_1_1,
  GYN_MENOPAUSE_2_1,
  LIFESTYLE_OUTDOOR_SUM_HRS_1_1,
  LIFESTYLE_OUTDOOR_WIN_HRS_1_1,
  LIFESTYLE_SCREEN_PC_HRS_2_1,
  LIFESTYLE_SCREEN_TV_HRS_2_1,
  SMOKE_EXPOSURE_1_1,
  SMOKE_EXPOSURE_HRS_1_1,
  SMOKE_TOBACCO_TYPE_1_M,
  SMOKE_100_TIMES_2_1,
  SMOKE_STATUS_2_1,
  SMOKE_REG_DAY_2_1,
  SMOKE_PREV_REG_2_1,
  WORK_MANUAL_LABOUR_1_1,
  WORK_YRS_1_1,
  WORK_WALK_STAND_1_1
FROM questionnaire
""")

ed = spark.sql("""
SELECT
  PID,
  ARRIVALDATE,
  DIAG_01, DIAG_02, DIAG_03, DIAG_04, DIAG_05, DIAG_06,
  DIAG_07, DIAG_08, DIAG_09, DIAG_10, DIAG_11, DIAG_12
FROM nhse_eng_ed
""")

clinic = spark.sql("""
SELECT
  PID,
  HEIGHT,
  WEIGHT,
  WAIST
FROM clinic_measurements
""")

joined = (
  questionnaire
  .join(ed, on="PID", how="left")
  .join(clinic, on="PID", how="left")
)

final_df = joined.toPandas()

