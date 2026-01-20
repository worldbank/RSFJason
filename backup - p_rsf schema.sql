/*
 Navicat PostgreSQL Dump SQL

 Source Server         : RSF Jason
 Source Server Type    : PostgreSQL
 Source Server Version : 160000 (160000)
 Source Host           : w0lxpfigrsfpg01:5432
 Source Catalog        : RSFDev
 Source Schema         : p_rsf

 Target Server Type    : PostgreSQL
 Target Server Version : 160000 (160000)
 File Encoding         : 65001

 Date: 20/01/2026 17:01:03
*/


-- ----------------------------
-- Type structure for gtrgm
-- ----------------------------
DROP TYPE IF EXISTS "p_rsf"."gtrgm";
CREATE TYPE "p_rsf"."gtrgm" (
  INPUT = "p_rsf"."gtrgm_in",
  OUTPUT = "p_rsf"."gtrgm_out",
  INTERNALLENGTH = VARIABLE,
  CATEGORY = U,
  DELIMITER = ','
);
ALTER TYPE "p_rsf"."gtrgm" OWNER TO "postgres";

-- ----------------------------
-- Sequence structure for export_template_reports_export_template_report_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."export_template_reports_export_template_report_id_seq";
CREATE SEQUENCE "p_rsf"."export_template_reports_export_template_report_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for export_templates_export_template_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."export_templates_export_template_id_seq";
CREATE SEQUENCE "p_rsf"."export_templates_export_template_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for exporting_cohorts_exporting_cohort_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."exporting_cohorts_exporting_cohort_id_seq";
CREATE SEQUENCE "p_rsf"."exporting_cohorts_exporting_cohort_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for import_templates_import_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."import_templates_import_id_seq";
CREATE SEQUENCE "p_rsf"."import_templates_import_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for indicator_check_formulas_check_formula_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."indicator_check_formulas_check_formula_id_seq";
CREATE SEQUENCE "p_rsf"."indicator_check_formulas_check_formula_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for indicator_check_guidance_guidance_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."indicator_check_guidance_guidance_id_seq";
CREATE SEQUENCE "p_rsf"."indicator_check_guidance_guidance_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for indicator_checks_check_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."indicator_checks_check_id_seq";
CREATE SEQUENCE "p_rsf"."indicator_checks_check_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for indicator_formulas_formula_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."indicator_formulas_formula_id_seq";
CREATE SEQUENCE "p_rsf"."indicator_formulas_formula_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for indicator_object_logs_log_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."indicator_object_logs_log_id_seq";
CREATE SEQUENCE "p_rsf"."indicator_object_logs_log_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for indicator_option_groups_option_group_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."indicator_option_groups_option_group_id_seq";
CREATE SEQUENCE "p_rsf"."indicator_option_groups_option_group_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for indicators_indicator_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."indicators_indicator_id_seq";
CREATE SEQUENCE "p_rsf"."indicators_indicator_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for label_ids_label_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."label_ids_label_id_seq";
CREATE SEQUENCE "p_rsf"."label_ids_label_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for labels_label_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."labels_label_id_seq";
CREATE SEQUENCE "p_rsf"."labels_label_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for reporting_templates_template_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."reporting_templates_template_id_seq";
CREATE SEQUENCE "p_rsf"."reporting_templates_template_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for reports_report_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."reports_report_id_seq";
CREATE SEQUENCE "p_rsf"."reports_report_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for rsf_data_calculation_profiles_calculation_profile_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."rsf_data_calculation_profiles_calculation_profile_id_seq";
CREATE SEQUENCE "p_rsf"."rsf_data_calculation_profiles_calculation_profile_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for rsf_data_checks_evaluation_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."rsf_data_checks_evaluation_id_seq";
CREATE SEQUENCE "p_rsf"."rsf_data_checks_evaluation_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for rsf_data_cohort_sequence
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."rsf_data_cohort_sequence";
CREATE SEQUENCE "p_rsf"."rsf_data_cohort_sequence" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for rsf_data_data_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."rsf_data_data_id_seq";
CREATE SEQUENCE "p_rsf"."rsf_data_data_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for rsf_pfcbl_ids_rsf_pfcbl_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."rsf_pfcbl_ids_rsf_pfcbl_id_seq";
CREATE SEQUENCE "p_rsf"."rsf_pfcbl_ids_rsf_pfcbl_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for rsf_program_facility_template_headers_header_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."rsf_program_facility_template_headers_header_id_seq";
CREATE SEQUENCE "p_rsf"."rsf_program_facility_template_headers_header_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Table structure for !dep-deleted_reporting_cohorts
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."!dep-deleted_reporting_cohorts";
CREATE TABLE "p_rsf"."!dep-deleted_reporting_cohorts" (
  "reporting_cohort_id" int4 NOT NULL,
  "upload_filename" text COLLATE "pg_catalog"."default" NOT NULL,
  "upload_file" text COLLATE "pg_catalog"."default",
  "reporting_asof_date" date NOT NULL,
  "reporting_rsf_pfcbl_id" int4 NOT NULL,
  "reporting_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "deleting_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "data_asof_date" date NOT NULL,
  "source_note" text COLLATE "pg_catalog"."default",
  "deleted_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone
)
;

-- ----------------------------
-- Table structure for !dep-rsf_pfcbl_id_family
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."!dep-rsf_pfcbl_id_family";
CREATE TABLE "p_rsf"."!dep-rsf_pfcbl_id_family" (
  "parent_pfcbl_category" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "parent_rsf_pfcbl_id" int4 NOT NULL,
  "child_rsf_pfcbl_id" int4 NOT NULL,
  "child_pfcbl_category" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "parent_pfcbl_rank" int2 NOT NULL,
  "child_pfcbl_rank" int2 NOT NULL
)
;

-- ----------------------------
-- Table structure for !dep-rsf_program_reporting_dates
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."!dep-rsf_program_reporting_dates";
CREATE TABLE "p_rsf"."!dep-rsf_program_reporting_dates" (
  "rsf_program_id" int4 NOT NULL,
  "valid_reporting_date" date NOT NULL,
  "reporting_sequence_rank" int4 NOT NULL,
  "rsf_pfcbl_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for _temp_headers
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."_temp_headers";
CREATE TABLE "p_rsf"."_temp_headers" (
  "sys_name" text COLLATE "pg_catalog"."default",
  "template_id" int4,
  "header_id" int4,
  "template_header_sheet_name" text COLLATE "pg_catalog"."default",
  "template_header" text COLLATE "pg_catalog"."default",
  "action" text COLLATE "pg_catalog"."default",
  "comment" text COLLATE "pg_catalog"."default",
  "map_indicator_id" int4,
  "map_formula_id" int4,
  "map_check_formula_id" int4
)
;

-- ----------------------------
-- Table structure for export_template_reports
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."export_template_reports";
CREATE TABLE "p_rsf"."export_template_reports" (
  "export_template_report_id" int4 NOT NULL DEFAULT nextval('"p_rsf".export_template_reports_export_template_report_id_seq'::regclass),
  "export_template_id" int4 NOT NULL,
  "report_id" int4,
  "sheet_name" varchar(30) COLLATE "pg_catalog"."default" NOT NULL,
  "table_name" varchar(30) COLLATE "pg_catalog"."default" NOT NULL
)
;

-- ----------------------------
-- Table structure for export_templates
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."export_templates";
CREATE TABLE "p_rsf"."export_templates" (
  "export_template_id" int4 NOT NULL DEFAULT nextval('"p_rsf".export_templates_export_template_id_seq'::regclass),
  "template_title" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "template_file" bytea,
  "template_filename" varchar(125) COLLATE "pg_catalog"."default",
  "template_notes" text COLLATE "pg_catalog"."default",
  "created_by_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "modified_by_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "modification_time" timestamptz(6) NOT NULL DEFAULT now(),
  "is_public" bool NOT NULL DEFAULT false
)
;

-- ----------------------------
-- Table structure for exporting_cohorts
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."exporting_cohorts";
CREATE TABLE "p_rsf"."exporting_cohorts" (
  "exporting_cohort_id" int4 NOT NULL DEFAULT nextval('"p_rsf".exporting_cohorts_exporting_cohort_id_seq'::regclass),
  "rsf_program_id" int4 NOT NULL,
  "exporting_rsf_pfcbl_id" int4 NOT NULL,
  "exporting_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "exporting_time" timestamp(6) NOT NULL DEFAULT now(),
  "export_name" varchar(255) COLLATE "pg_catalog"."default",
  "reporting_expiration_date" date DEFAULT ((now() + '14 days'::interval))::date,
  "reporting_key" text COLLATE "pg_catalog"."default" NOT NULL,
  "generated_with_report_id" int4,
  "for_reporting_template_id" int4 NOT NULL,
  "data_integrity_key" text COLLATE "pg_catalog"."default" NOT NULL,
  "exporting_asof_date" date
)
;
COMMENT ON COLUMN "p_rsf"."exporting_cohorts"."exporting_rsf_pfcbl_id" IS 'parent rsf reporting ID';

-- ----------------------------
-- Table structure for indicator_check_formula_parameters
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_check_formula_parameters";
CREATE TABLE "p_rsf"."indicator_check_formula_parameters" (
  "indicator_check_id" int4 NOT NULL,
  "check_formula_id" int4 NOT NULL,
  "for_pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
  "check_grouping_pfcbl_category" text COLLATE "pg_catalog"."default",
  "check_grouping_pfcbl_rank" int2,
  "parameter_indicator_id" int4 NOT NULL,
  "parameter_pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
  "parameter_pfcbl_rank" int2 NOT NULL,
  "parameter_pfcbl_hierarchy" text COLLATE "pg_catalog"."default" NOT NULL,
  "is_calculation_trigger_parameter" bool NOT NULL DEFAULT false,
  "parameter_trigger_by_reporting" bool NOT NULL DEFAULT false
)
;
COMMENT ON COLUMN "p_rsf"."indicator_check_formula_parameters"."parameter_pfcbl_hierarchy" IS '-1 = parent; 0 = self; 1 = child';
COMMENT ON COLUMN "p_rsf"."indicator_check_formula_parameters"."is_calculation_trigger_parameter" IS 'true if its used in the FORMULA calculation (ie, an input data should trigger it to stale) whereas a MESSAGE parameter should not trigger a recalculation but is nevertheless a parameter that must be obtained on query along with the associated rsf_pfcbl_id entity';

-- ----------------------------
-- Table structure for indicator_check_formulas
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_check_formulas";
CREATE TABLE "p_rsf"."indicator_check_formulas" (
  "indicator_check_id" int4 NOT NULL,
  "formula" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'TRUE==TRUE'::text,
  "formula_result_message" text COLLATE "pg_catalog"."default",
  "formula_indicator_ids" int4[] NOT NULL DEFAULT ARRAY[]::integer[],
  "formula_comments" text COLLATE "pg_catalog"."default",
  "formula_version_number" int4 NOT NULL DEFAULT 0,
  "formula_modification_time" timestamp(6) NOT NULL DEFAULT now(),
  "auto_resolve" bool NOT NULL DEFAULT false,
  "check_pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
  "check_pfcbl_rank" int2 NOT NULL,
  "parameter_pfcbl_ranks" int2[] NOT NULL,
  "parent_grouping_pfcbl_rank" int2,
  "parent_grouping_pfcbl_category" text COLLATE "pg_catalog"."default",
  "check_formula_indicator_ids" int4[],
  "check_message_indicator_ids" int4[],
  "computation_group" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT ''::text,
  "formula_fx_date" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'calculation'::text,
  "check_formula_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_check_formulas_check_formula_id_seq'::regclass),
  "check_formula_title" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'Untitled check formula'::text,
  "default_subscription" bool NOT NULL DEFAULT false,
  "label_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."indicator_check_formulas"."parameter_pfcbl_ranks" IS 'Includes parameter ranks, only';
COMMENT ON COLUMN "p_rsf"."indicator_check_formulas"."parent_grouping_pfcbl_rank" IS 'For grouping and/or child-level parameters';

-- ----------------------------
-- Table structure for indicator_check_guidance
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_check_guidance";
CREATE TABLE "p_rsf"."indicator_check_guidance" (
  "indicator_check_guidance_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_check_guidance_guidance_id_seq'::regclass),
  "indicator_check_id" int4 NOT NULL,
  "for_indicator_id" int4 NOT NULL,
  "guidance" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'No guidance specified'::text,
  "is_resolving_guidance" bool NOT NULL DEFAULT false,
  "user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "update_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone,
  "for_pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
  "overwrite_check_class" text COLLATE "pg_catalog"."default",
  "is_ignoring_guidance" bool NOT NULL DEFAULT false,
  "variance_threshold" numeric NOT NULL DEFAULT 0.0,
  "is_priority_flag" bool NOT NULL DEFAULT false,
  "check_formula_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."indicator_check_guidance"."is_resolving_guidance" IS 'When set to true, enables users to refine check behavior to auto-resolve for any combination of check_ids and indicator_ids, using the specified guidance as the auto-resolving resolution comment and user_id as the auto-resolving user';
COMMENT ON COLUMN "p_rsf"."indicator_check_guidance"."variance_threshold" IS 'For checks that have a variance threshold, if the variance is within the specified threshold then this guidance will be aplied';
COMMENT ON COLUMN "p_rsf"."indicator_check_guidance"."check_formula_id" IS 'TBD: Do we care more about the application or the calculation; or both?';

-- ----------------------------
-- Table structure for indicator_check_types
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_check_types";
CREATE TABLE "p_rsf"."indicator_check_types" (
  "check_type" text COLLATE "pg_catalog"."default" NOT NULL,
  "check_type_name" text COLLATE "pg_catalog"."default",
  "check_type_notes" text COLLATE "pg_catalog"."default",
  "apply_on" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'data'::text
)
;
COMMENT ON COLUMN "p_rsf"."indicator_check_types"."apply_on" IS 'Either ''data'' or ''reporting'' -- is the check applied onto a specific data_id data point or the data_id generated for the sys_x_reporting that is generaically generated';

-- ----------------------------
-- Table structure for indicator_checks
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_checks";
CREATE TABLE "p_rsf"."indicator_checks" (
  "indicator_check_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_checks_check_id_seq'::regclass),
  "check_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "check_class" varchar(255) COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'warning'::character varying,
  "grouping" varchar(255) COLLATE "pg_catalog"."default",
  "subgrouping" varchar(255) COLLATE "pg_catalog"."default",
  "definition" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT ''::text,
  "is_system" bool NOT NULL DEFAULT false,
  "version_number" int4 NOT NULL DEFAULT 0,
  "modification_time" timestamp(6) NOT NULL DEFAULT now(),
  "variance_tolerance_allowed" bool NOT NULL DEFAULT false,
  "is_calculator_check" bool NOT NULL DEFAULT false,
  "check_type" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'none'::text,
  "check_pfcbl_category" text COLLATE "pg_catalog"."default",
  "auto_resolve_system_check" bool,
  "auto_subscribe" bool DEFAULT true
)
;
COMMENT ON COLUMN "p_rsf"."indicator_checks"."variance_tolerance_allowed" IS 'When true, indicates a % variance from an existing value and if outside that tolerance range, will apply the flag; and if not, flag ignored.  Only relevant for system checks, notably system calculator overwrites or other "disagreement" type flags.  Enabled through custom guidance application';
COMMENT ON COLUMN "p_rsf"."indicator_checks"."check_pfcbl_category" IS 'Null means is_system=true since a single system flag can be applied on any data point';

-- ----------------------------
-- Table structure for indicator_data_types
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_data_types";
CREATE TABLE "p_rsf"."indicator_data_types" (
  "data_type" text COLLATE "pg_catalog"."default" NOT NULL
)
;

-- ----------------------------
-- Table structure for indicator_data_types_currency_units
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_data_types_currency_units";
CREATE TABLE "p_rsf"."indicator_data_types_currency_units" (
  "currency_code" varchar(3) COLLATE "pg_catalog"."default" NOT NULL,
  "currency_name" text COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for indicator_formula_parameters
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_formula_parameters";
CREATE TABLE "p_rsf"."indicator_formula_parameters" (
  "indicator_id" int4 NOT NULL,
  "calculate_pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
  "calculate_grouping_pfcbl_category" text COLLATE "pg_catalog"."default",
  "calculate_grouping_pfcbl_rank" int2,
  "parameter_indicator_id" int4 NOT NULL,
  "parameter_pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
  "parameter_pfcbl_rank" int2 NOT NULL,
  "parameter_pfcbl_hierarchy" text COLLATE "pg_catalog"."default" NOT NULL,
  "parameter_is_current" bool NOT NULL DEFAULT false,
  "parameter_is_previous" bool NOT NULL DEFAULT false,
  "parameter_is_info" bool NOT NULL DEFAULT false,
  "parameter_is_all" bool NOT NULL DEFAULT false,
  "parameter_trigger_by_reporting" bool NOT NULL DEFAULT false,
  "parameter_data_type" text COLLATE "pg_catalog"."default" NOT NULL,
  "formula_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for indicator_formulas
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_formulas";
CREATE TABLE "p_rsf"."indicator_formulas" (
  "indicator_id" int4 NOT NULL,
  "formula" text COLLATE "pg_catalog"."default",
  "formula_sort" text COLLATE "pg_catalog"."default",
  "overwrite" varchar(255) COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'allow'::character varying,
  "formula_indicator_ids" int4[],
  "formula_indicator_id_requirements" int4[],
  "formula_indicator_id_dependents" int4[],
  "formula_calculation_rank" int2 NOT NULL DEFAULT 0,
  "formula_grouping_pfcbl_rank" int2,
  "formula_pfcbl_rank_range" int2[],
  "dep-formula_calculated_by_indicator_id" int4,
  "perform_calculation_by_row" bool DEFAULT false,
  "modification_time" timestamp(6) NOT NULL DEFAULT now(),
  "formula_fx_date" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'calculation'::text,
  "computation_group" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT ''::text,
  "computation_priority_rank" int2 NOT NULL DEFAULT 0,
  "formula_unit_set_by_indicator_id" int4,
  "formula_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_formulas_formula_id_seq'::regclass),
  "formula_title" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'Untitled Formula'::text,
  "is_primary_default" bool NOT NULL DEFAULT true,
  "formula_notes" text COLLATE "pg_catalog"."default",
  "modified_by_user_id" text COLLATE "pg_catalog"."default"
)
;
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."overwrite" IS 'when system calculator can overwite a user-submitted value.  Default is to allow overwrites, assuming system calculator is most accurate and consistent: allow, deny, missing, unchanged';
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."formula_indicator_ids" IS 'Indicator IDs required to calculate the formula';
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."formula_indicator_id_requirements" IS 'All prerequisite data points';
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."formula_indicator_id_dependents" IS 'All calculations that will be invalidated by changes to this indicator';
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."perform_calculation_by_row" IS 'System will group "by" rsf_pfcbl_category when indicators of different categories are presented; but if only one category is presented, system will not use any grouping at all UNLESS perform_calculation_by_row=TRUE and then group at row-level or NULL (auto group).  This allows using aggregate functions within a category row-space.  Eg, sum(loan_outstanding,loan_comitted) whereas without the grouping, sum() would aggregate over the entire dataset and surely yield a wrong value.  Row level calculations also needed when using functions that are not vectorized; performs a similar function as lapply() would to loop over values, but keeps formula definition much cleaner.
TRUE = Force row level grouping within auto-groups
FALSE = No grouping
NA = Auto ';
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."formula_fx_date" IS 'Either "calculation" or "parameter"
"calculation" will use the FX rate date of the latest updated parameter that is used in the calculation (max parameter update date)

Whereas "parameter" will use the FX rate date for each individual parameter''s update date; for aggregated calculations, this may mean for example that an fx rate across different time periods would be used in constructing the overall aggrate value in the output fx terms.';
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."computation_priority_rank" IS 'Zero is low priority.  Used to segment formula_calculation_ranks into sub-ranks for independent calculation.  Practically, is used to calculate currency_ratio calculations first since other subsequent calculations may rely on these (or not) in an unknowable way depending on whether any given formula must undertake an fx conversion due to its input data, as determined at the computation time.';
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."formula_unit_set_by_indicator_id" IS 'When set, output result will be set to the unit of the relevant indicator.  To ensure a 1:1 relationship that won''t conflict, the unit must be equal or lower pfcbl_rank to the formula''s rank.  Indicators must also be of same data_type,  When not set, it will default to the data_unit of the indicator; and for LCU values, will default to the entity''s LCU specification.';

-- ----------------------------
-- Table structure for indicator_object_logs
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_object_logs";
CREATE TABLE "p_rsf"."indicator_object_logs" (
  "log_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_object_logs_log_id_seq'::regclass),
  "table_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "table_id" int4 NOT NULL,
  "log_date" date NOT NULL DEFAULT (now())::date,
  "version_number" int4 NOT NULL DEFAULT 1,
  "log_object" jsonb NOT NULL DEFAULT '{}'::jsonb
)
;

-- ----------------------------
-- Table structure for indicator_options_group_keys
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_options_group_keys";
CREATE TABLE "p_rsf"."indicator_options_group_keys" (
  "options_group_id" int4 NOT NULL,
  "options_group_key" varchar(10) COLLATE "pg_catalog"."default" NOT NULL,
  "label_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for indicator_options_groups
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_options_groups";
CREATE TABLE "p_rsf"."indicator_options_groups" (
  "options_group_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_option_groups_option_group_id_seq'::regclass),
  "options_group_name" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "options_group_definition" text COLLATE "pg_catalog"."default",
  "options_group_data_type" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'text'::text
)
;

-- ----------------------------
-- Table structure for indicator_sys_categories
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_sys_categories";
CREATE TABLE "p_rsf"."indicator_sys_categories" (
  "indicator_sys_category" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "is_hidden" bool NOT NULL DEFAULT false,
  "comment" text COLLATE "pg_catalog"."default",
  "class" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "category_cascade" bool NOT NULL DEFAULT false
)
;
COMMENT ON COLUMN "p_rsf"."indicator_sys_categories"."is_hidden" IS 'note: not yet implemented';

-- ----------------------------
-- Table structure for indicators
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicators";
CREATE TABLE "p_rsf"."indicators" (
  "indicator_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicators_indicator_id_seq'::regclass),
  "indicator_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "indicator_sys_category" varchar(255) COLLATE "pg_catalog"."default",
  "data_category" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "data_type" varchar COLLATE "pg_catalog"."default" NOT NULL,
  "data_unit" varchar(255) COLLATE "pg_catalog"."default",
  "default_value" varchar(255) COLLATE "pg_catalog"."default",
  "definition" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'Undefined'::text,
  "label_id" int4 NOT NULL,
  "indicator_options_group_id" int4,
  "indicator_options_group_allows_blanks" bool,
  "indicator_options_group_allows_multiples" bool,
  "is_calculated" bool NOT NULL DEFAULT false,
  "is_system" bool NOT NULL DEFAULT false,
  "is_data_unit" bool NOT NULL DEFAULT false,
  "is_system_calculated" bool NOT NULL DEFAULT false,
  "is_setup" text COLLATE "pg_catalog"."default",
  "modification_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone,
  "version_number" int4 NOT NULL DEFAULT 1,
  "is_static_nonreporting" bool NOT NULL DEFAULT false,
  "default_subscription" bool NOT NULL DEFAULT false,
  "is_periodic_or_flow_reporting" bool NOT NULL DEFAULT false,
  "classification" text COLLATE "pg_catalog"."default",
  "sort_preference" int2,
  "created_by_user_id" text COLLATE "pg_catalog"."default",
  "modified_by_user_id" text COLLATE "pg_catalog"."default",
  "pfcbl_rank" int2 NOT NULL
)
;
COMMENT ON COLUMN "p_rsf"."indicators"."default_value" IS 'If an entity submits a column with an NA value, then use a default, if defined.  But if entity never submits any value (ever), then db_program_get_data will return NA';
COMMENT ON COLUMN "p_rsf"."indicators"."is_system" IS 'true when indicator is created within/by system processes, not externally defined';
COMMENT ON COLUMN "p_rsf"."indicators"."is_data_unit" IS 'If true, the data_value recorded in rsf_data is a unit of measure (which presumably defines the units of other data and is used in rsf_data_timeseries::data_unit_data_id)';
COMMENT ON COLUMN "p_rsf"."indicators"."is_system_calculated" IS 'If true calculated by the system independently AND uses the formula definition in indicator_formulas; if false, calcualted by the normal system calculator -- this is needed as queries to indicator formulas will pull these in and is_system_calculated informs the calculator to ignore these.  Note: that some system indicators are indeed calculated by the system, but do not have is_system_calculated=true because the are calculated entirely outside of any indicator_formula definition';
COMMENT ON COLUMN "p_rsf"."indicators"."is_setup" IS 'true when it''s a required field use to setup/initialize an entity; fields to present to UI';
COMMENT ON COLUMN "p_rsf"."indicators"."is_static_nonreporting" IS 'When true, BEFORE INSERT on rsf_data will DENY any inclusion of these indicators.  And therefore no values will be saved in rsf_data nor present in rsf_data_timeseries.

Rather, values for these indicators are calculated on-demand ad-hoc within the application later in the rsf_program_get_data() function.

The reason for this is that these indicators have values that might change beyond the reporting timelines of the entities that are capturing them.  For example, the REPORTING_STATUS that is determined relative to the reporting_asof_date for which reporting_status is requested (rather than relative to the last known value, as is the case with all reported data in rsf_data).';
COMMENT ON COLUMN "p_rsf"."indicators"."default_subscription" IS 'Application layer will submit first in a partial cohort to trigger rsf_data_timeseries to only manage this indicator data first (this is used by currency unit reporting to ensure that if updates to currency data units will exist before other data on the same timeline so rsf_data_timeseries interprets their LCU data units according to the current update in the same timeline)';
COMMENT ON COLUMN "p_rsf"."indicators"."is_periodic_or_flow_reporting" IS 'Overwhelmingly, data is STOCK data and static (or implicitly interpreted to be stock, such as a loan repayment can be interpreted as a single "last payment made" rather than a flow of payments over time).  This flags data that are flow: such as the QDD date or other data that are explicilty flow data associated with the reporting period.  

As of 2023, this  metric is NOT used by the calculator or database to determine changes.  Eg, if a flow indicator reports $100 this quarter and next quarter also reports $100, it will be discarded as a non-change.  Currently, as these indicators are rare it presents no issues.  This may be a todo item to update';

-- ----------------------------
-- Table structure for label_ids
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."label_ids";
CREATE TABLE "p_rsf"."label_ids" (
  "label_id" int4 NOT NULL DEFAULT nextval('"p_rsf".label_ids_label_id_seq'::regclass),
  "label_id_group" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "label_definition" text COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for label_keys
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."label_keys";
CREATE TABLE "p_rsf"."label_keys" (
  "label_key" varchar(25) COLLATE "pg_catalog"."default" NOT NULL,
  "label_key_name" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "key_type" varchar(15) COLLATE "pg_catalog"."default" NOT NULL,
  "key_type_template_id" int4
)
;

-- ----------------------------
-- Table structure for labels
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."labels";
CREATE TABLE "p_rsf"."labels" (
  "label_id" int4 NOT NULL,
  "label_key" varchar(25) COLLATE "pg_catalog"."default" NOT NULL,
  "primary_label" text COLLATE "pg_catalog"."default" NOT NULL,
  "secondary_labels" text[] COLLATE "pg_catalog"."default" NOT NULL DEFAULT ARRAY[]::text[],
  "label_id_group" varchar(255) COLLATE "pg_catalog"."default" NOT NULL
)
;

-- ----------------------------
-- Table structure for program_settings
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."program_settings";
CREATE TABLE "p_rsf"."program_settings" (
  "setting_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "default_value" text COLLATE "pg_catalog"."default" NOT NULL,
  "setting_group" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'none'::text,
  "default_data_type" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'text'::text,
  "definition" text COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for reporting_cohort_info
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reporting_cohort_info";
CREATE TABLE "p_rsf"."reporting_cohort_info" (
  "reporting_cohort_id" int4 NOT NULL,
  "metadata" jsonb NOT NULL DEFAULT '{}'::jsonb,
  "upload_filename" text COLLATE "pg_catalog"."default",
  "upload_file" bytea,
  "data_count_reported" int4 NOT NULL DEFAULT 0,
  "data_count_calculated" int4 NOT NULL DEFAULT 0,
  "data_current_count_reported" int4 NOT NULL DEFAULT 0,
  "data_current_count_calculated" int4 NOT NULL DEFAULT 0
)
;

-- ----------------------------
-- Table structure for reporting_cohorts
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reporting_cohorts";
CREATE TABLE "p_rsf"."reporting_cohorts" (
  "reporting_cohort_id" int4 NOT NULL DEFAULT nextval('"p_rsf".rsf_data_cohort_sequence'::regclass),
  "reporting_asof_date" date NOT NULL,
  "reporting_rsf_pfcbl_id" int4 NOT NULL,
  "reporting_user_id" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'UNKNOWN'::text,
  "reporting_time" timestamptz(6) NOT NULL DEFAULT now(),
  "!dep-source_name" text COLLATE "pg_catalog"."default",
  "!dep-source_reference" text COLLATE "pg_catalog"."default",
  "!dep-source_note" text COLLATE "pg_catalog"."default",
  "!dep-rsf_program_id" int4,
  "!dep-parent_reporting_cohort_id" int4,
  "is_calculated_cohort" bool NOT NULL DEFAULT false,
  "is_reported_cohort" bool NOT NULL DEFAULT true,
  "!dep-is_redundancy_cohort" bool DEFAULT false,
  "!dep-from_reporting_template_id" int4,
  "!dep-linked_reporting_cohort_id" int4,
  "!dep-cohort_processing_completed" bool DEFAULT true,
  "data_asof_date" date NOT NULL,
  "!dep-rsf_facility_id" int4,
  "import_id" int4,
  "reporting_type" int2 NOT NULL DEFAULT 0
)
;
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."reporting_user_id" IS 'References: ARL.arlapplications.accounts.account_id';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."!dep-rsf_program_id" IS 'program for which cohort is reporting';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."!dep-parent_reporting_cohort_id" IS 'For system calculator and other cohorts that report "under" a primary cohort';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."!dep-is_redundancy_cohort" IS 'Generally the result of a data entry error in excel: when the same entity repeats multiple different information on different rows.  The latest (highest row number) is interepted as the current data, whereas data on lower row numbers are saved for change log recording in a redundancy cohort.';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."!dep-linked_reporting_cohort_id" IS 'Linked reporting cohort does NOT have fk to reporting_asof_date.  Its used by system-created cohorts and backfills that are triggerd by newer reporting cohorts that create cohorts on different reporting timelines to ensure a complete reporting timeseries is available for calculations and checks.';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."data_asof_date" IS 'If the data timestamp is not quarter end, precisely.';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."reporting_type" IS '0=Setup/System, 1=User Import, 2=Calculator';

-- ----------------------------
-- Table structure for reporting_import_template_headers
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reporting_import_template_headers";
CREATE TABLE "p_rsf"."reporting_import_template_headers" (
  "import_id" int4 NOT NULL,
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_id" int4 NOT NULL,
  "template_header" text COLLATE "pg_catalog"."default" NOT NULL,
  "template_header_position" text COLLATE "pg_catalog"."default" NOT NULL
)
;

-- ----------------------------
-- Table structure for reporting_imports
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reporting_imports";
CREATE TABLE "p_rsf"."reporting_imports" (
  "import_id" int4 NOT NULL DEFAULT (nextval('"p_rsf".import_templates_import_id_seq'::regclass))::integer,
  "import_rsf_pfcbl_id" int4 NOT NULL,
  "import_pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
  "import_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "import_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone,
  "import_completed" bool NOT NULL DEFAULT false,
  "reporting_asof_date" date NOT NULL,
  "template_id" int4 NOT NULL,
  "file_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "file_data" bytea NOT NULL,
  "is_finalized" bool NOT NULL DEFAULT false,
  "finalized_by_user_id" text COLLATE "pg_catalog"."default",
  "finalized_time" timestamptz(6),
  "import_comments" text COLLATE "pg_catalog"."default",
  "finalized_comments" text COLLATE "pg_catalog"."default",
  "pfcbl_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "metadata" jsonb NOT NULL DEFAULT '{}'::jsonb
)
;

-- ----------------------------
-- Table structure for reporting_imports_deleted_archive
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reporting_imports_deleted_archive";
CREATE TABLE "p_rsf"."reporting_imports_deleted_archive" (
  "import_id" int4 NOT NULL,
  "import_rsf_pfcbl_id" int4 NOT NULL,
  "file_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "file_data" bytea,
  "reporting_asof_date" date NOT NULL,
  "import_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "import_comments" text COLLATE "pg_catalog"."default",
  "deleting_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "deleted_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone
)
;

-- ----------------------------
-- Table structure for reporting_templates
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reporting_templates";
CREATE TABLE "p_rsf"."reporting_templates" (
  "template_id" int4 NOT NULL DEFAULT nextval('"p_rsf".reporting_templates_template_id_seq'::regclass),
  "template_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL DEFAULT ARRAY[]::integer[],
  "template_key" varchar(255) COLLATE "pg_catalog"."default" NOT NULL DEFAULT upper(md5((now())::text)),
  "description" text COLLATE "pg_catalog"."default",
  "is_reportable" bool NOT NULL DEFAULT false,
  "template_has_static_row_ids" bool NOT NULL DEFAULT false,
  "is_setup_template" bool NOT NULL DEFAULT false,
  "file_extension" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'xlsx'::text,
  "is_system" bool NOT NULL DEFAULT false
)
;
COMMENT ON COLUMN "p_rsf"."reporting_templates"."template_has_static_row_ids" IS 'Template format reliably reports same entry on same row, always adding new entries to new rows.  Ie, row 1 will always report the same entity, etc';

-- ----------------------------
-- Table structure for reports
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reports";
CREATE TABLE "p_rsf"."reports" (
  "report_id" int4 NOT NULL DEFAULT nextval('"p_rsf".reports_report_id_seq'::regclass),
  "created_by_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "is_public" bool NOT NULL,
  "report_title" text COLLATE "pg_catalog"."default" NOT NULL,
  "report_notes" text COLLATE "pg_catalog"."default",
  "for_program_sys_name" text COLLATE "pg_catalog"."default",
  "for_facility_sys_names" jsonb,
  "for_indicator_names" jsonb,
  "for_asof_dates" jsonb,
  "report_parameters" jsonb NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_clients
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_clients";
CREATE TABLE "p_rsf"."rsf_clients" (
  "rsf_client_id" int4 NOT NULL,
  "rsf_facility_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_data
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data";
CREATE TABLE "p_rsf"."rsf_data" (
  "data_id" int4 NOT NULL DEFAULT nextval('"p_rsf".rsf_data_data_id_seq'::regclass),
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_id" int4 NOT NULL,
  "reporting_asof_date" date NOT NULL,
  "reporting_cohort_id" int4 NOT NULL,
  "data_value" text COLLATE "pg_catalog"."default",
  "data_unit" text COLLATE "pg_catalog"."default",
  "data_submitted" text COLLATE "pg_catalog"."default",
  "data_source_row_id" varchar(128) COLLATE "pg_catalog"."default",
  "data_sys_flags" int2,
  "data_sys_source" int2 NOT NULL DEFAULT 0
)
;
COMMENT ON COLUMN "p_rsf"."rsf_data"."data_submitted" IS 'Whatever was submitted via user input or sys (whereas data_value is the data of record and potentially normalized)';
COMMENT ON COLUMN "p_rsf"."rsf_data"."data_source_row_id" IS 'Row number (and/or reference) from uploaded Excel template that corresponds to reporting_cohort_id.  NULL are system inputs that have no explicit or meaningful row_id';
COMMENT ON COLUMN "p_rsf"."rsf_data"."data_sys_flags" IS 'Flags:
0: reserved
1: reserved
2: deleted (won''t be present in rsf_data_current)
4: manual overwrite (if calculated, accept; no overwrite)
6: "reverted" ie, delete current and restore previously reported value.
8: ';
COMMENT ON COLUMN "p_rsf"."rsf_data"."data_sys_source" IS '0=Undefined; 1=Reported Cohort; 2=System Calculation Cohort; 3=System Generated';

-- ----------------------------
-- Table structure for rsf_data_calculation_evaluations
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_calculation_evaluations";
CREATE TABLE "p_rsf"."rsf_data_calculation_evaluations" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_id" int4 NOT NULL,
  "calculation_asof_date" date NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_data_calculation_validations
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_calculation_validations";
CREATE TABLE "p_rsf"."rsf_data_calculation_validations" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_id" int4 NOT NULL,
  "calculation_asof_date" date NOT NULL,
  "data_id" int4 NOT NULL,
  "validation_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone
)
;

-- ----------------------------
-- Table structure for rsf_data_check_evaluations
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_check_evaluations";
CREATE TABLE "p_rsf"."rsf_data_check_evaluations" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "check_asof_date" date NOT NULL,
  "check_formula_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_data_checks
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_checks";
CREATE TABLE "p_rsf"."rsf_data_checks" (
  "evaluation_id" int4 NOT NULL DEFAULT nextval('"p_rsf".rsf_data_checks_evaluation_id_seq'::regclass),
  "data_id" int4 NOT NULL,
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_id" int4 NOT NULL,
  "check_asof_date" date NOT NULL,
  "indicator_check_id" int4 NOT NULL,
  "status_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone,
  "check_message" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'Check does not defined message.  Update in settings'::text,
  "check_status" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'active'::character varying,
  "check_status_comment" text COLLATE "pg_catalog"."default",
  "check_status_user_id" text COLLATE "pg_catalog"."default",
  "check_ignore" bool NOT NULL DEFAULT false,
  "indicator_check_guidance_id" int4,
  "check_data_id_is_current" bool NOT NULL,
  "check_formula_id" int4,
  "consolidated_from_indicator_id" int4,
  "consolidated_from_indicator_check_id" int4,
  "data_sys_flags" int2,
  "data_value_unit" text COLLATE "pg_catalog"."default",
  "archive_sys_name" text COLLATE "pg_catalog"."default"
)
;
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."check_ignore" IS 'TODO: remove this and consolidate "ignore" fully under guidance';
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."data_sys_flags" IS 'Flags on rsf_data are set via flagging';

-- ----------------------------
-- Table structure for rsf_data_checks_archive
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_checks_archive";
CREATE TABLE "p_rsf"."rsf_data_checks_archive" (
  "archive_id" int4 NOT NULL,
  "archive_time" timestamptz(6),
  "sys_name" text COLLATE "pg_catalog"."default",
  "rsf_pfcbl_id" int4,
  "indicator_id" int4,
  "indicator_check_id" int4,
  "check_formula_id" int4,
  "check_asof_date" date,
  "check_status" text COLLATE "pg_catalog"."default",
  "status_time" timestamptz(6),
  "check_status_user_id" text COLLATE "pg_catalog"."default",
  "check_status_comment" text COLLATE "pg_catalog"."default",
  "check_message" text COLLATE "pg_catalog"."default",
  "consolidated_from_indicator_id" int4,
  "consolidated_from_indicator_check_id" int4,
  "data_sys_flags" int2,
  "data_value_unit" text COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for rsf_data_current
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_current";
CREATE TABLE "p_rsf"."rsf_data_current" (
  "data_id" int4 NOT NULL,
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_id" int4 NOT NULL,
  "reporting_asof_date" date NOT NULL,
  "data_value" text COLLATE "pg_catalog"."default",
  "data_unit" text COLLATE "pg_catalog"."default",
  "data_unit_data_id" int4
)
;

-- ----------------------------
-- Table structure for rsf_data_current_fx
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_current_fx";
CREATE TABLE "p_rsf"."rsf_data_current_fx" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_id" int4 NOT NULL,
  "reporting_asof_date" date NOT NULL,
  "fx_data_id" int4 NOT NULL
)
;
COMMENT ON COLUMN "p_rsf"."rsf_data_current_fx"."fx_data_id" IS 'Current FX data value USED for this calculation';

-- ----------------------------
-- Table structure for rsf_data_current_lcu
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_current_lcu";
CREATE TABLE "p_rsf"."rsf_data_current_lcu" (
  "lcu_unit_data_id" int4 NOT NULL,
  "for_rsf_pfcbl_id" int4 NOT NULL,
  "reporting_asof_date" date NOT NULL,
  "data_unit_value" text COLLATE "pg_catalog"."default" NOT NULL,
  "data_id_pfcbl_rank" int2 NOT NULL,
  "is_defined_lcu" bool NOT NULL
)
;
COMMENT ON COLUMN "p_rsf"."rsf_data_current_lcu"."data_id_pfcbl_rank" IS 'Entity will inheret an LCU value that is greater than its own rank and lower than any rank that''s currently there.  Eg, if current data_id is from Program and its Facility submits a value, it will be accepted; and if it then submits a value it will be accepted -- but if the facility then updates its value it will not since it already has a lower value present.';
COMMENT ON COLUMN "p_rsf"."rsf_data_current_lcu"."is_defined_lcu" IS 'If an entity submits a non-null defined unit (defined currency unit can only be applied at equal pfcbl_rank) then no inherited values can be accepted after that reporting date.';

-- ----------------------------
-- Table structure for rsf_data_current_names_and_ids
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_current_names_and_ids";
CREATE TABLE "p_rsf"."rsf_data_current_names_and_ids" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "reporting_asof_date" date NOT NULL,
  "sys_name" text COLLATE "pg_catalog"."default",
  "id" text COLLATE "pg_catalog"."default",
  "name" text COLLATE "pg_catalog"."default",
  "nickname" text COLLATE "pg_catalog"."default",
  "rank_id" text COLLATE "pg_catalog"."default",
  "pfcbl_category" text COLLATE "pg_catalog"."default",
  "pfcbl_name" text COLLATE "pg_catalog"."default",
  "tranche_id" text COLLATE "pg_catalog"."default",
  "series_id" text COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for rsf_facilities
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_facilities";
CREATE TABLE "p_rsf"."rsf_facilities" (
  "rsf_facility_id" int4 NOT NULL,
  "rsf_program_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_loan_issuance_series
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_loan_issuance_series";
CREATE TABLE "p_rsf"."rsf_loan_issuance_series" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "loan_issuance_series_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "loan_issuance_series_rank" int4 NOT NULL,
  "id_value_data_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_pfcbl_categories
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_pfcbl_categories";
CREATE TABLE "p_rsf"."rsf_pfcbl_categories" (
  "pfcbl_category" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "pfcbl_rank" int2
)
;

-- ----------------------------
-- Table structure for rsf_pfcbl_ids
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_pfcbl_ids";
CREATE TABLE "p_rsf"."rsf_pfcbl_ids" (
  "rsf_pfcbl_id" int4 NOT NULL DEFAULT (nextval('"p_rsf".rsf_pfcbl_ids_rsf_pfcbl_id_seq'::regclass))::integer,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "rsf_client_id" int4,
  "rsf_borrower_id" int4,
  "rsf_loan_id" int4,
  "pfcbl_category" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "pfcbl_category_rank" int2 NOT NULL,
  "created_by_reporting_cohort_id" int4 NOT NULL DEFAULT 0,
  "created_in_reporting_asof_date" date NOT NULL,
  "created_timestamp" timestamp(6) NOT NULL DEFAULT now(),
  "!dep-parent_rsf_pfcbl_id" int4,
  "!dep-rsf_pf_id" int4,
  "deactivated_in_reporting_asof_date" date,
  "deactivated_by_reporting_cohort_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."rsf_pfcbl_ids"."created_by_reporting_cohort_id" IS 'Zero value indicates parse_template created new rsf_pfcbl_ids and related rsf_ids but as-yet didn''t upload any data or create a cohort to claim these IDs.  If something faltered mid-upload, these IDs are effectively null and should be cleaned-up.  When claimed, cohort will also provide creation timestamp and uploader user_id etc';
COMMENT ON COLUMN "p_rsf"."rsf_pfcbl_ids"."created_in_reporting_asof_date" IS 'Convenient field joins to reporting_cohorts''s rsf_reporting reporting_asof_date, set by parse_template''s claim cohort ';
COMMENT ON COLUMN "p_rsf"."rsf_pfcbl_ids"."!dep-parent_rsf_pfcbl_id" IS 'TO BE DEPRECATED - SEP2020 along with rsf_pfcbl_id_family';
COMMENT ON COLUMN "p_rsf"."rsf_pfcbl_ids"."!dep-rsf_pf_id" IS 'TO BE DEPRECATED - SEP2020 along with rsf_pfcbl_id_family';

-- ----------------------------
-- Table structure for rsf_pfcbl_reporting
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_pfcbl_reporting";
CREATE TABLE "p_rsf"."rsf_pfcbl_reporting" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "reporting_asof_date" date NOT NULL,
  "created_by_data_id" int4 NOT NULL,
  "reporting_indicator_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_pfcbl_reporting_template_row_ids
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_pfcbl_reporting_template_row_ids";
CREATE TABLE "p_rsf"."rsf_pfcbl_reporting_template_row_ids" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "import_id" int4,
  "data_source_row_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "template_id" int4 NOT NULL,
  "parent_rsf_pfcbl_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_program_facility_check_guidance
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_program_facility_check_guidance";
CREATE TABLE "p_rsf"."rsf_program_facility_check_guidance" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_check_guidance_id" int4 NOT NULL,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "applied_by_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "application_time" timestamptz(6) NOT NULL
)
;
COMMENT ON COLUMN "p_rsf"."rsf_program_facility_check_guidance"."rsf_facility_id" IS 'Null when guidance is set at the program level';

-- ----------------------------
-- Table structure for rsf_program_facility_template_headers
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_program_facility_template_headers";
CREATE TABLE "p_rsf"."rsf_program_facility_template_headers" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "template_id" int4 NOT NULL,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "header_id" int4 NOT NULL DEFAULT nextval('"p_rsf".rsf_program_facility_template_headers_header_id_seq'::regclass),
  "template_header_sheet_name" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT ''::text,
  "template_header" text COLLATE "pg_catalog"."default" NOT NULL,
  "template_header_encounter_index" int2 NOT NULL DEFAULT 0,
  "action" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'default'::text,
  "action_mapping" text COLLATE "pg_catalog"."default" NOT NULL,
  "comment" text COLLATE "pg_catalog"."default",
  "map_indicator_id" int4,
  "map_formula_id" int4,
  "map_check_formula_id" int4,
  "template_header_full_normalized" text COLLATE "pg_catalog"."default" NOT NULL
)
;
COMMENT ON COLUMN "p_rsf"."rsf_program_facility_template_headers"."template_header_sheet_name" IS ''''' means all/any sheets encountered';
COMMENT ON COLUMN "p_rsf"."rsf_program_facility_template_headers"."template_header_encounter_index" IS '0 means uniquely used (majority, hopefully!)';
COMMENT ON COLUMN "p_rsf"."rsf_program_facility_template_headers"."action" IS 'default, ignore, remap';
COMMENT ON COLUMN "p_rsf"."rsf_program_facility_template_headers"."action_mapping" IS 'to enable unique constraint';

-- ----------------------------
-- Table structure for rsf_program_settings
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_program_settings";
CREATE TABLE "p_rsf"."rsf_program_settings" (
  "rsf_program_id" int4 NOT NULL,
  "setting_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "setting_value" text COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for rsf_programs
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_programs";
CREATE TABLE "p_rsf"."rsf_programs" (
  "rsf_program_id" int4 NOT NULL,
  "reporting_period" varchar(255) COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'quarter'::character varying
)
;

-- ----------------------------
-- Table structure for rsf_setup_checks
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_setup_checks";
CREATE TABLE "p_rsf"."rsf_setup_checks" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "check_formula_id" int4 NOT NULL,
  "indicator_check_id" int4 NOT NULL,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "is_subscribed" bool NOT NULL,
  "is_auto_subscribed" bool NOT NULL DEFAULT false,
  "subscription_comments" text COLLATE "pg_catalog"."default",
  "comments_user_id" text COLLATE "pg_catalog"."default",
  "auto_subscribed_by_reporting_cohort_id" int4
)
;

-- ----------------------------
-- Table structure for rsf_setup_checks_config
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_setup_checks_config";
CREATE TABLE "p_rsf"."rsf_setup_checks_config" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "for_indicator_id" int4 NOT NULL,
  "indicator_check_id" int4 NOT NULL,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "config_auto_resolve" bool NOT NULL,
  "config_check_class" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT false,
  "config_threshold" numeric NOT NULL DEFAULT 0.0,
  "config_comments" text COLLATE "pg_catalog"."default",
  "comments_user_id" text COLLATE "pg_catalog"."default",
  "auto_subscribed_by_reporting_cohort_id" int4
)
;

-- ----------------------------
-- Table structure for rsf_setup_indicators
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_setup_indicators";
CREATE TABLE "p_rsf"."rsf_setup_indicators" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_id" int4 NOT NULL,
  "formula_id" int4,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "is_subscribed" bool NOT NULL,
  "is_auto_subscribed" bool NOT NULL DEFAULT false,
  "sort_preference" int2,
  "subscription_comments" text COLLATE "pg_catalog"."default",
  "comments_user_id" text COLLATE "pg_catalog"."default",
  "options_group_id" int4,
  "formula_calculation_unit" text COLLATE "pg_catalog"."default",
  "auto_subscribed_by_reporting_cohort_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."rsf_setup_indicators"."formula_id" IS 'If it''s a calculated indicator (or possibly not?), the default (program) or specific formula (facility) is using to calculate this indicator''s value';
COMMENT ON COLUMN "p_rsf"."rsf_setup_indicators"."is_subscribed" IS 'True: Yes, proactively monitored; False: No, proactively not monitored; Null: Reported and therefore implicitly monitored';
COMMENT ON COLUMN "p_rsf"."rsf_setup_indicators"."is_auto_subscribed" IS 'System will set as auto subscribed at the program level when it''s reported (and delete entries that are auto subscribed only if no entries remain after a dataset delete)';
COMMENT ON COLUMN "p_rsf"."rsf_setup_indicators"."sort_preference" IS 'Display sorting in RSF Setup and Reporting';
COMMENT ON COLUMN "p_rsf"."rsf_setup_indicators"."formula_calculation_unit" IS 'This is only used for currency units.

This can only be set on currency data type indicators that have LCU as their defined currency unit (indicators with specific currencies, like USD) must return those values.

If an invalid currency unit is entered, it will given a calculation error message.';

-- ----------------------------
-- Function structure for cascade_check_subgrouping_changes_to_formulas
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."cascade_check_subgrouping_changes_to_formulas"();
CREATE FUNCTION "p_rsf"."cascade_check_subgrouping_changes_to_formulas"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN
-- TODO rewrite this: Basically a subgrouping version number change will cause update trigger to fire on indicator_check_formulas and 
-- incorporate new IDs from subgroupings.
	if (new.subgrouping is distinct from old.subgrouping)
	then 
		update p_rsf.indicator_check_formulas 
		set formula_version_number = formula_version_number+1 
		where indicator_check_id = new.indicator_check_id;
	end if;
	
	return NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for cascade_indicator_name_changes_to_formulas
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."cascade_indicator_name_changes_to_formulas"();
CREATE FUNCTION "p_rsf"."cascade_indicator_name_changes_to_formulas"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  -- if no change then don't both cascading
  if (NEW.indicator_name = OLD.indicator_name) then
	  return NEW;
	end if;
	
  
  -- update before appending "." to names since template headers should specify the actual indicator name (not, eg, indicator_name.current)
  update p_rsf.rsf_program_facility_template_headers fth  
		set template_header = regexp_replace(fth.template_header,OLD.indicator_name,NEW.indicator_name,'g')
	where fth.template_header is not null
	  and fth.template_header ~ (OLD.indicator_name); 	

  -- all formulas expect to use parameter-style indicator name references, whose values are noted by a period (.).
	-- For example: indicator_name.current or indicator_name.all.dates, etc
	-- Add period to ensure entire indicator name is matched.
	-- Eg if indicatorA is: "indicator_name_ABC" and indicatorB is "indicator_name" then indicatorB would partially and incorrectly match
	-- indicatorA.  However, "indicator_name." will not match "indicator_name_ABC."
	
  OLD.indicator_name := OLD.indicator_name || '\.'; -- match against literal period at end of old indicator in formula
	NEW.indicator_name := NEW.indicator_name || '.'; -- not matched, so add literal period to end of new indicator in formula
	
	update p_rsf.indicator_formulas indf
		set formula = regexp_replace(formula,OLD.indicator_name,NEW.indicator_name,'g')
	where indf.formula is not null               -- null will replace with indicator_name and violate self-refferential constraint
	  and indf.formula ~ (OLD.indicator_name);   -- ensure full name with indicator_name.attribute format, which is used by formulas
		
	update p_rsf.indicator_formulas indf
		set formula_sort = regexp_replace(formula_sort,OLD.indicator_name,NEW.indicator_name,'g')
	where indf.formula_sort is not null            -- null will replace with indicator_name and violate self-refferential constraint
	  and indf.formula_sort ~ (OLD.indicator_name); -- ensure full name with indicator_name.attribute format, which is used by formulas
																							

  update p_rsf.indicator_check_formulas icf
		set formula = regexp_replace(formula,OLD.indicator_name,NEW.indicator_name,'g')
	where icf.formula is not null
	  and icf.formula ~ (OLD.indicator_name); 

  update p_rsf.indicator_check_formulas icf
		set formula_result_message = regexp_replace(formula_result_message,OLD.indicator_name,NEW.indicator_name,'g')
	where icf.formula_result_message is not null
	  and icf.formula_result_message ~ (OLD.indicator_name); 

  update p_rsf.indicator_checks ic
		set subgrouping = regexp_replace(subgrouping,OLD.indicator_name,NEW.indicator_name,'g')
	where ic.subgrouping is not null
	  and ic.subgrouping ~ (OLD.indicator_name); 	
	
	RETURN NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for cascade_indicator_subscriptions
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."cascade_indicator_subscriptions"();
CREATE FUNCTION "p_rsf"."cascade_indicator_subscriptions"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  -- Maybe make a separate table in the future?  But the refresh takes just milliseconds.
	REFRESH MATERIALIZED VIEW p_rsf.mview_rsf_program_indicator_cascades;
	RETURN NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for cascade_label_id_group
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."cascade_label_id_group"();
CREATE FUNCTION "p_rsf"."cascade_label_id_group"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

	select label_id_group into NEW.label_id_group 
	from p_rsf.label_ids where label_ids.label_id = NEW.label_id;
	
	return NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for check_valid_guidance_entry
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."check_valid_guidance_entry"();
CREATE FUNCTION "p_rsf"."check_valid_guidance_entry"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

	if exists(select * from p_rsf.indicator_check_guidance this_icg
						where this_icg.indicator_check_guidance_id = NEW.indicator_check_guidance_id
						  and this_icg.for_pfcbl_category in ('global','program')) 
	then 
		 NEW.rsf_facility_id := NULL;
	end if;

	if exists(select * 
	          from p_rsf.indicator_check_guidance this_icg
						inner join p_rsf.indicator_check_guidance other_icg on other_icg.for_indicator_id = this_icg.for_indicator_id
						                                                   and other_icg.indicator_check_id = this_icg.indicator_check_id
						where this_icg.indicator_check_guidance_id = NEW.indicator_check_guidance_id
						  and other_icg.indicator_check_guidance_id <> this_icg.indicator_check_guidance_id
						  and exists(select * from p_rsf.rsf_program_facility_check_guidance pfcg
												 where pfcg.indicator_check_guidance_id = other_icg.indicator_check_guidance_id
												   and pfcg.rsf_program_id = NEW.rsf_program_id
													 and pfcg.rsf_facility_id is not distinct from NEW.rsf_facility_id)) then
    raise exception 'Unable to add guidance as this program/facility is already subscribed to another relevant guidance.';
		return NULL;
	end if;
		/*
	if not exists(select * from p_rsf.rsf_pfcbl_ids ids
	              where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id
								  and ids.rsf_program_id = NEW.rsf_program_id
									and ids.rsf_facility_id is not distinct from NEW.rsf_facility_id) then
	  raise exception 'Invalid subscription for rsf_pfcbl_id=%, rsf_program_id=%, rsf_facility_id=%',
		                NEW.rsf_pfcbl_id, NEW.rsf_program_id, NEW.rsf_facility_id;
	  return NULL;
	end if;
	*/
	if NEW.rsf_facility_id is NULL
		 and not exists(select * from p_rsf.indicator_check_guidance this_icg
										where this_icg.indicator_check_guidance_id = NEW.indicator_check_guidance_id
										  and this_icg.for_pfcbl_category in ('global','program')) then
    raise exception 'Missing rsf_facility_id not allowed for this guidance is not at the global/program level: indicator_check_guidance_id=%',
		NEW.indicator_check_guidance_id;
		return NULL;
	end if;
									
	return NEW;
END; 
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for check_valid_guidance_pfcbl_category
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."check_valid_guidance_pfcbl_category"();
CREATE FUNCTION "p_rsf"."check_valid_guidance_pfcbl_category"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE indicator_pfcbl_category text;
BEGIN

  if (TG_OP = 'UPDATE' AND coalesce(NEW.for_pfcbl_category <> OLD.for_pfcbl_category,true) = true) then
		raise exception 'Guidance for_pfcbl_category cannot change from "%" once set',
		OLD.for_pfcbl_category;
		return NULL;		
	end if;
	
	if (NEW.for_pfcbl_category is NULL)
	then
		select ind.data_category
		into NEW.for_pfcbl_category
		from p_rsf.indicators ind
		where ind.indicator_id = NEW.for_indicator_id;
	end if;
	
	if (NEW.for_pfcbl_category NOT in ('global','program','facility'))
	then
		NEW.for_pfcbl_category := 'facility';
	end if;
										
	return NEW;
END; 
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for cohort_linked_consistency_check
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."cohort_linked_consistency_check"();
CREATE FUNCTION "p_rsf"."cohort_linked_consistency_check"()
  RETURNS "pg_catalog"."trigger" AS $BODY$

BEGIN

	if exists(select * from p_rsf.rsf_pfcbl_ids ids 
						where ids.rsf_pfcbl_id = NEW.reporting_rsf_pfcbl_id)
		 and not exists(select * from p_rsf.rsf_pfcbl_ids ids 
										where ids.rsf_pfcbl_id = NEW.reporting_rsf_pfcbl_id
									    and ids.created_in_reporting_asof_date <= NEW.reporting_asof_date)
  then
		raise exception 'reporting_rsf_pfcbl_id % does not exist for this reporting_asof_date %',
		(NEW.reporting_rsf_pfcbl_id),(NEW.reporting_asof_date);
		
	end if;									
	
  if (NEW.linked_reporting_cohort_id is NOT NULL)
	then
		
		if not exists(select * from p_rsf.reporting_cohorts rc
		              where rc.reporting_cohort_id = NEW.linked_reporting_cohort_id
									  and rc.reporting_rsf_pfcbl_id is not distinct from NEW.reporting_rsf_pfcbl_id
										and rc.rsf_program_id is not distinct from NEW.rsf_program_id
										and rc.reporting_asof_date is distinct from NEW.reporting_asof_date)
        and
        not exists(select * from p_rsf.reporting_cohorts rc
                   where rc.reporting_cohort_id = NEW.linked_reporting_cohort_id
                     and rc.rsf_program_id is not distinct from NEW.rsf_program_id
                     and rc.rsf_facility_id is distinct from NEW.rsf_facility_id)
    then
			raise exception 'linked cohorts must have same reporting_rsf_pfcbl_id and different reporting_asof_dates 
                       OR
                       same rsf_program_id and different rsf_facility_id
                       If reporting requires the same reporting_asof_date then create as a parent cohort instead of a linked cohort';
		end if;										
		
	end if;
	
	return NEW;
END; 
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for data_value_is_meaningfully_different
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."data_value_is_meaningfully_different"("input_rsf_pfcbl_id" int4, "input_indicator_id" int4, "input_reporting_asof_date" date, "input_data_value" text, "input_data_unit" text, "compare_data_source_row_id" text, "is_redundancy_reporting" bool);
CREATE FUNCTION "p_rsf"."data_value_is_meaningfully_different"("input_rsf_pfcbl_id" int4, "input_indicator_id" int4, "input_reporting_asof_date" date, "input_data_value" text, "input_data_unit" text, "compare_data_source_row_id" text=NULL::text, "is_redundancy_reporting" bool=false)
  RETURNS "pg_catalog"."bool" AS $BODY$
declare existing_data_id int;
declare existing_data_value text;
declare existing_data_unit text;
declare existing_data_unit_current text;
declare existing_data_reporting_asof_date date;
declare existing_data_cohort_id int;
declare different bool;
declare flow_data bool default false;
declare flow_data_category text;
BEGIN

  --v_current_data_sequence_rank:=1;
	input_data_value := trim(input_data_value);
	input_data_unit := upper(trim(input_data_unit));
	/*
	--rdc is queried because rdc has normalzied data units for currency
	select
		rd.data_value,
		rd.data_unit,  -- IMPORTANT: rd.data_unit and NOT rdc.data_unit because rd.data_unit is what is submitted, and for LCU/LCY, rdc is adjusted!!
		rdc.data_id,
		rdc.data_unit, -- because rsf_data_current changes data units based on LCU values and may more may not be equal to rd.data_unit as a result
		rdc.reporting_asof_date,
		rd.reporting_cohort_id
		into 
		existing_data_value, existing_data_unit, existing_data_id, existing_data_unit_current, existing_data_reporting_asof_date, existing_data_cohort_id
	from p_rsf.rsf_data_current rdc
	inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
	where rdc.rsf_pfcbl_id = input_rsf_pfcbl_id
		and rdc.indicator_id = input_indicator_id
		and rdc.reporting_asof_date <= input_reporting_asof_date
	order by rdc.reporting_asof_date desc --rdc.data_id desc
	limit 1;
*/

	--rdc is queried because rdc has normalzied data units for currency
	select
		rd.data_value,
		rd.data_unit,  -- IMPORTANT: rd.data_unit and NOT rdc.data_unit because rd.data_unit is what is submitted, and for LCU/LCY, rdc is adjusted!!
		coalesce(rdc.data_id,rd.data_id) as data_id,
		
		-- because rsf_data_current changes data units based on LCU values and may more may not be equal to rd.data_unit as a result
		coalesce(rdc.data_unit,rd.data_unit) as data_unit, 
		rd.reporting_asof_date,
		rd.reporting_cohort_id
		into 
		existing_data_value, existing_data_unit, existing_data_id, existing_data_unit_current, existing_data_reporting_asof_date, existing_data_cohort_id
	from p_rsf.rsf_data rd
	left join p_rsf.rsf_data_current rdc on rdc.data_id = rd.data_id
	where rd.rsf_pfcbl_id = input_rsf_pfcbl_id
		and rd.indicator_id = input_indicator_id
		and rd.reporting_asof_date <= input_reporting_asof_date
		and (coalesce(rd.data_sys_flags,0) & 2)=0
	order by rd.reporting_asof_date desc,rdc.data_id is not null desc,rd.data_id desc
	limit 1;


	
	 -- no entry exists whatsoever, so any entry, even NULL is meaningfully new
	 if existing_data_id is NULL then
	   return true;
	 end if;

   different := (existing_data_value is distinct from input_data_value)  -- different value
								or 
								(existing_data_unit_current is distinct from input_data_unit AND -- different 
								 existing_data_unit is distinct from input_data_unit);
   /*
	 raise notice 'Data is meaningfully different: % and 
	               existing_data_value=% existing_data_unit=% existing_data_id=% existing_data_unit_current=% existing_data_reporting_asof_date=%',
		different,
		existing_data_value, existing_data_unit, existing_data_id, existing_data_unit_current, existing_data_reporting_asof_date;
	 */
	 
	 -- if the currentest value is a redundancy reporting and this is not redundancy reporting, then it doesn't matter
		 -- if the previous row already reported: we need to ensure that redundancy data gets cleared out of being current.
	 if is_redundancy_reporting = false
			AND exists(select * from p_rsf.reporting_cohorts rc
								 where rc.reporting_cohort_id = existing_data_cohort_id
									 and rc.is_redundancy_cohort = true)
	 then
				return true;
	 end if;
		 
	 if different = false AND is_redundancy_reporting = false
	 then
	 
	   select coalesce(ind.is_periodic_or_flow_reporting,false),ind.data_category
		 into flow_data,flow_data_category
		 from p_rsf.indicators ind
		 where ind.indicator_id = input_indicator_id;
		 
		 
		 --raise notice 'Data is meaningfully different: % and flow_data=% flow_data_category=%',different,flow_data,flow_data_category;
		 
		 -- flow data is by definition a measure of a change over time
		 -- so if last time reported NULL then nothing changed.  And if reporting again NULL then it's spam so reject it.
		 -- Whereas reporting 0 last time and 0 again can indicate an actual quantity of something that (didn't) happen in the period.
		 if flow_data = true 
			  AND NULLIF(existing_data_value,'0') is NULL
				AND NULLIF(input_data_value,'0') is NULL
		 then
			
			different := false;
			return (different);
			
		 elseif flow_data = true 
		 		-- in case, eg, a borrower has a flow data and multiple reporting entries that won't spam each vaule
		    AND existing_data_reporting_asof_date is distinct from input_reporting_asof_date
				-- if its in active we don't want to spam it with zeros or nulls.
				-- but inactive loans may still report recoveries.
		    AND (NULLIF(input_data_value,'0') is NOT NULL
				     OR
						 (NULLIF(existing_data_value,'0') is NOT NULL AND NULLIF(input_data_value,'0') is NULL)
				     OR
						 exists(select * from p_rsf.get_rsf_pfcbl_id_reporting_status_asof_date(input_rsf_pfcbl_id,
				                                                                            flow_data_category,
																																									  input_reporting_asof_date) as status
							         where status.quarter_reporting_expected = true)
						)
		 then
		   different := true;
			 return (different);
		 end if;
		 
	 end if;									
   -- we don't have a source_row_id at all, so only check if the data or unit has changed	 
	 if compare_data_source_row_id is NULL then
	 
			return different;

		-- return existing_data_value is distinct from input_data_value OR
		--				existing_data_unit is distinct from input_data_unit;
	 
	 -- a data_source_row_id is provided
	 else 
		 -- a data_source_row_id is provided BUT the test input unit and value are the same as the most recently reported values
		 -- regardless of whatever its source_row_id is
		 -- Therefore: no change.
		 if different = false then
		   return false;
		 end if;
		 
		 /*
		 -- because rdc.data_value can be deleted if the redundancy vlaue is identical to the historic value
		 -- and then the next value is changed -- but also might be repeated by data_source_row_id and therefore return as not different
		 -- but it is different as compared to the redundancy data that has been deleted because it is identical to the historic value.
		 if different = true
		    AND existing_data_reporting_asof_date is distinct from input_reporting_asof_date
				AND is_redundancy_reporting = false
     then 
			  return true;
		 end if;			 
		 */
		 -- a data_source_row_id is provided AND data value and/or unit are different
		 -- but check if they're different from the last time this row reported?
		 select
			 rd.data_value,
			 rd.data_unit  -- IMPORTANT: rd.data_unit and NOT rdc.data_unit because rd.data_unit is what is submitted, and for LCU/LCY, rdc is adjusted!!
			 
			 into 
			 -- NOTE: existing_data_id is NOT updated here, data_id remains latest-added in time/sequence
			 existing_data_value, existing_data_unit
		 from p_rsf.rsf_data rd 
		 where rd.rsf_pfcbl_id = input_rsf_pfcbl_id
			 and rd.indicator_id = input_indicator_id
			 and rd.reporting_asof_date <= input_reporting_asof_date
			 and rd.data_source_row_id = compare_data_source_row_id
			 and (coalesce(rd.data_sys_flags,0) & 2)=0 -- BIT 2 = SOFT DELETE FLAG
		 --order by rdc.data_id desc
		 order by rd.reporting_asof_date desc, rd.data_id desc
		 limit 1;
		
		-- if it is different, then insert the change
		if existing_data_value is distinct from input_data_value OR
			 existing_data_unit is distinct from input_data_unit
		then
					 
				return true;
    else
				return false;
		end if;
	 
	 end if;
	 
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for data_value_is_meaningfully_different2
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."data_value_is_meaningfully_different2"("input_rsf_pfcbl_id" int4, "input_indicator_id" int4, "input_reporting_asof_date" date, "input_data_value" text, "input_data_unit" text, "is_user_reporting" bool);
CREATE FUNCTION "p_rsf"."data_value_is_meaningfully_different2"("input_rsf_pfcbl_id" int4, "input_indicator_id" int4, "input_reporting_asof_date" date, "input_data_value" text, "input_data_unit" text, "is_user_reporting" bool=false)
  RETURNS "pg_catalog"."bool" AS $BODY$
declare existing_data_id int;
declare existing_data_value text;
declare existing_data_unit text;
declare existing_data_unit_current text;
declare existing_data_reporting_asof_date date;
declare existing_data_cohort_id int;
declare different bool;
--declare flow_data bool default false;
--declare flow_data_category text;
BEGIN

  --v_current_data_sequence_rank:=1;
	input_data_value := trim(input_data_value);
	input_data_unit := upper(trim(input_data_unit));

	--rdc is queried because rdc has normalzied data units for currency
	select
		rd.data_value,
		rd.data_unit,  -- IMPORTANT: rd.data_unit and NOT rdc.data_unit because rd.data_unit is what is submitted, and for LCU/LCY, rdc is adjusted!!
		coalesce(rdc.data_id,rd.data_id) as data_id,
		
		-- because rsf_data_current changes data units based on LCU values and may more may not be equal to rd.data_unit as a result
		coalesce(rdc.data_unit,rd.data_unit) as data_unit, 
		rd.reporting_asof_date,
		rd.reporting_cohort_id
		into 
		existing_data_value, existing_data_unit, existing_data_id, existing_data_unit_current, existing_data_reporting_asof_date, existing_data_cohort_id
	from p_rsf.rsf_data rd  
	left join p_rsf.rsf_data_current rdc on rdc.data_id = rd.data_id
	where rd.rsf_pfcbl_id = input_rsf_pfcbl_id
		and rd.indicator_id = input_indicator_id
		and rd.reporting_asof_date <= input_reporting_asof_date
		and (coalesce(rd.data_sys_flags,0) & 2)=0
	order by rd.reporting_asof_date desc,rdc.data_id is not null desc,rd.data_id desc
	limit 1;


	
	 -- no entry exists whatsoever, so any entry, even NULL is meaningfully new
	 if existing_data_id is NULL then
	   return true;
	 end if;

   different := (existing_data_value is distinct from input_data_value)  -- different value
								or 
								(existing_data_unit_current is distinct from input_data_unit AND -- different 
								 existing_data_unit is distinct from input_data_unit);
   /*
	 raise notice 'Data is meaningfully different: % and 
	               existing_data_value=% existing_data_unit=% existing_data_id=% existing_data_unit_current=% existing_data_reporting_asof_date=%',
		different,
		existing_data_value, existing_data_unit, existing_data_id, existing_data_unit_current, existing_data_reporting_asof_date;
	 */
	 
	 -- if the currentest value is a redundancy reporting and this is not redundancy reporting, then it doesn't matter
		 -- if the previous row already reported: we need to ensure that redundancy data gets cleared out of being current.
   
   -- Its NOT different...
   -- But is it a "flow" data?
   -- If so, one can reasonably report the same magnitude of change consecutively.
	 if different = false
      AND exists(select * from p_rsf.indicators ind 
                 where ind.indicator_id = input_indicator_id
                   and ind.is_periodic_or_flow_reporting is TRUE)
	 then
	 
		 
		 -- flow data is by definition a measure of a change over time
		 -- so if last time reported NULL then nothing changed.  And if reporting again NULL then it's spam so reject it.
		 -- Whereas reporting 0 last time and 0 again can indicate an actual quantity of something that (didn't) happen in the period.
		 if NULLIF(existing_data_value,'0') is NULL
				AND NULLIF(input_data_value,'0') is NULL
		 then
			
			--different := false;
			return (false);
			
		 elseif existing_data_reporting_asof_date is distinct from input_reporting_asof_date
		 		-- in case, eg, a borrower has a flow data and multiple reporting entries that won't spam each vaule
				-- if its in active we don't want to spam it with zeros or nulls.
				-- but inactive loans may still report recoveries.
		    AND (NULLIF(input_data_value,'0') is NOT NULL
				     OR
						 (NULLIF(existing_data_value,'0') is NOT NULL AND NULLIF(input_data_value,'0') is NULL)
				     OR
             --THIS NEEDS TO CHANGE!!
						 exists(select * from p_rsf.get_rsf_pfcbl_id_reporting_status_asof_date(input_rsf_pfcbl_id,
				                                                                            (select ind.data_category from p_rsf.indicators ind
                                                                                     where ind.indicator_id = input_indicator_id),
																																									  input_reporting_asof_date) as status
							         where status.quarter_reporting_expected = true)
						)
		 then
		   --different := true;
			 return (true);
		 end if;
		 
	 end if;
   
   -- It's NOT flow data (flow data exits)
   -- If it's not different, then exit (source doesn't matter if it's unchanged)
   if different is false then
		   return false;
       
   -- if it's not user reported (ie, it's system/calcualted)
   -- then we only want to compare to the currentest, whether that's system or user data
	 elseif is_user_reporting is false
   then
       return (different); -- is true
   
   -- We aren't flow
   -- We are different
   -- We are user-reported
   -- And currentest data is NOT user-reported
   elseif exists(select *
                 from p_rsf.reporting_cohorts rc 
                 where rc.reporting_cohort_id = existing_data_cohort_id
                   and rc.is_reported_cohort is false)
   then
      -- Then let's look at the last user-reported data    
		 select
			 rd.data_value,
			 rd.data_unit  -- IMPORTANT: rd.data_unit and NOT rdc.data_unit because rd.data_unit is what is submitted, and for LCU/LCY, rdc is adjusted!!			 
			 into 
			 -- NOTE: existing_data_id is NOT updated here, data_id remains latest-added in time/sequence
			 existing_data_value, 
       existing_data_unit
		 from p_rsf.rsf_data rd 
     inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
		 where rd.rsf_pfcbl_id = input_rsf_pfcbl_id
			 and rd.indicator_id = input_indicator_id
			 and rd.reporting_asof_date <= input_reporting_asof_date
			 and rc.is_reported_cohort is true
			 and (coalesce(rd.data_sys_flags,0) & 2)=0 -- BIT 2 = SOFT DELETE FLAG
		 order by rd.reporting_asof_date desc, rd.data_id desc
		 limit 1;
     
    -- if it is different, then insert the change
		if existing_data_value is distinct from input_data_value OR
			 existing_data_unit is distinct from input_data_unit
		then					 
				return true;
    else
				return false;
		end if;
    
   -- We aren't flow
   -- We are different
   -- We are user-reported
   -- And currentest data IS user-reported
   else 
     return different; -- will be true
   end if;     	 
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for dblink_account_info
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."dblink_account_info"();
CREATE FUNCTION "p_rsf"."dblink_account_info"()
  RETURNS TABLE("account_id" text, "users_name" text, "login_email" text, "is_system_account" bool) AS $BODY$
BEGIN
return query
select 
accounts.account_id,
accounts.users_name,
accounts.login_email,
accounts.is_system_account
from dblink('dbname=ARL',
					  '
select vai.account_id,vai.users_name,vai.login_email,vai.is_system_account 
from arlapplications.view_account_info vai
where exists(select * from arlapplications.account_applications aa
             where aa.account_id = vai.account_id
						   and aa.application_hashid = (select ap.application_hashid from arlapplications.applications ap where ap.application_name = ''RSF JASON''))
')
					 as accounts(account_id text,users_name text,login_email text, is_system_account bool);
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100
  ROWS 1000;

-- ----------------------------
-- Function structure for delete_rsf_id
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."delete_rsf_id"();
CREATE FUNCTION "p_rsf"."delete_rsf_id"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

		delete from p_rsf.rsf_clients rc
		where exists(select * from deleted_rsf_data drd
		             where drd.rsf_pfcbl_id = rc.rsf_client_id
								   and drd.pfcbl_category = 'client');

		delete from p_rsf.rsf_facilities rf
		where exists(select * from deleted_rsf_data drd
		             where drd.rsf_pfcbl_id = rf.rsf_facility_id
								   and drd.pfcbl_category = 'facility');

		delete from p_rsf.rsf_programs rp
		where exists(select * from deleted_rsf_data drd
		             where drd.rsf_pfcbl_id = rp.rsf_program_id
								   and drd.pfcbl_category in ('program','global'));
    
		return NULL;									 
																			
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for deleted_reporting_imports_action
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."deleted_reporting_imports_action"();
CREATE FUNCTION "p_rsf"."deleted_reporting_imports_action"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE deleted_rsf_pf_id int;
BEGIN
  
	if (not exists(select * from newly_deleted))
	then
		return NULL;
	end if;

	delete from p_rsf.reporting_imports ri
	using newly_deleted
	where newly_deleted.import_id = ri.import_id;
	
	-- delete logged deletes after 100 days
	delete from p_rsf.reporting_imports_deleted_archive drc
	where (drc.deleted_time + interval '100 days') < TIMEOFDAY()::timestamptz;

	return NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for deleted_reporting_imports_logging
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."deleted_reporting_imports_logging"();
CREATE FUNCTION "p_rsf"."deleted_reporting_imports_logging"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN	
	
	perform users.rsf_pfcbl_id_validate_permissions(validate_account_id => NEW.deleting_user_id,
                                                  validate_rsf_pfcbl_id => (select ri.import_rsf_pfcbl_id
																									                          from p_rsf.reporting_imports ri
																																						where ri.import_id = NEW.import_id),
																									validate_permission_name => 'DELETE');	
		
	
	if exists(select * 
	          from p_rsf.reporting_imports ri
            where ri.import_id = NEW.import_id)
  then 
									
		select 
			ri.import_rsf_pfcbl_id,
      ri.file_name,
      ri.file_data,
      ri.reporting_asof_date,
      ri.import_user_id,
      ri.import_comments
		into
			NEW.import_rsf_pfcbl_id,
      NEW.file_name,
      NEW.file_data,
      NEW.reporting_asof_date,
      NEW.import_user_id,
      NEW.import_comments
		from p_rsf.reporting_imports ri
		where ri.import_id = NEW.import_id;

		NEW.deleted_time := timeofday()::timestamptz;
		
		return NEW;
		
  else 
		
		return NULL;
  end if;		
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for function_evaluate_calculations_using_modified_data
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."function_evaluate_calculations_using_modified_data"("event_type" text, "event_is_calculated_cohort" bool);
CREATE FUNCTION "p_rsf"."function_evaluate_calculations_using_modified_data"("event_type" text='update'::text, "event_is_calculated_cohort" bool=false)
  RETURNS "pg_catalog"."void" AS $BODY$
begin
     
  if (not event_type in ('insert','update','delete')) 
  then
    raise exception 'event_type must be: insert, update, delete; use update for manual resets or testing';
  end if;
  
  /*
  create temp table _sys_calculated_data(rsf_pfcbl_id int,
                                         indicator_id int,
                                         reporting_asof_date date)
  on commit drop;
  */
  
  raise info 'function_evaluate_calculations_using_modified_data(%,%)',event_type,event_is_calculated_cohort;
  
  --##validate pending evaluations when insert is by system calculator
  --  anything that is in serted in a calculated cohort must have been prompted by a subscribed calculation!
  --  and if somehow it isn't a subscribed calculation, we should definitely remove it!  So this seems unnecessary
  if (event_type = 'insert') and
     (event_is_calculated_cohort is true)
  then
  /*
        raise info 'function_evaluate_calculations_using_modified_data(%,%) DELETING %',event_type,event_is_calculated_cohort,
        (select count(*) from p_rsf.rsf_data_calculation_evaluations dce
         where exists(select * from _modified_data md 
                      where md.rsf_pfcbl_id = dce.rsf_pfcbl_id
                        and md.indicator_id = dce.indicator_id
                        and md.reporting_asof_date = dce.calculation_asof_date));
*/                        
        delete from p_rsf.rsf_data_calculation_evaluations dce
        using _modified_data md
        where dce.rsf_pfcbl_id = md.rsf_pfcbl_id
          and dce.indicator_id = md.indicator_id
          and dce.calculation_asof_date = md.reporting_asof_date;  
    
  end if;
  
  with reporting_formula_subscriptions as (
    select 
      rpr.rsf_pfcbl_id,
      rpr.reporting_asof_date,
      sis.indicator_id,
      sis.formula_id
    from p_rsf.rsf_pfcbl_reporting rpr
    inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = rpr.rsf_pfcbl_id
                                                               and sis.filter_matched_pfcbl_indicators is true 
                                                               -- because this would pair all indicators across their reporting hierarchies
                                                               -- eg, we may end up with a facility indicator being paired with the global entity, etc.
                                                               -- note: other subscription references join on indicator_id as well, which serves this filter matching
    where sis.is_subscribed is true
      and sis.is_calculated is true
      
      and exists(select * from _modified_data mcd
                 where mcd.data_id = rpr.created_by_data_id) -- rpr.created_by_data_id has an index
  ),
  parameter_triggers as (
          
    select 
      mcd.rsf_pfcbl_id as parameter_pfcbl_id,
      ifp.indicator_id as calculate_indicator_id,
      mcd.reporting_asof_date,
      ifp.parameter_pfcbl_category,
      ifp.calculate_pfcbl_category,
      ifp.formula_id,
      ifp.parameter_pfcbl_hierarchy
    from _modified_data mcd                                   
    inner join p_rsf.indicator_formula_parameters ifp on ifp.parameter_indicator_id = mcd.indicator_id
    
    union 
    
    select
      rfs.rsf_pfcbl_id as parameter_pfcbl_id,
      ifp.indicator_id as calculate_indicator_id,
      rfs.reporting_asof_date,
      ifp.parameter_pfcbl_category,
      ifp.calculate_pfcbl_category,
      ifp.formula_id,
      ifp.parameter_pfcbl_hierarchy
    from reporting_formula_subscriptions rfs
    inner join p_rsf.indicator_formula_parameters ifp on ifp.formula_id = rfs.formula_id
    where ifp.parameter_trigger_by_reporting = true 
    -- this is basically for formulas that use .previous or .all (ie, use historical data when current data may not be reported)
  ),
  
  -- because we cannot join on to_family_rsf_pfcbl_id without materializing it; else the indexes will fail and the query will take forever. Vastly faster if materialized.
  calculation_parameter_triggers_mat as materialized (
    select distinct
    calc.calculate_indicator_id,
    ft.to_family_rsf_pfcbl_id as calculate_rsf_pfcbl_id,
    calc.reporting_asof_date as calculation_asof_date,
    calc.formula_id
    from parameter_triggers calc
    inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = calc.parameter_pfcbl_id
                                                     and ft.to_pfcbl_category = calc.calculate_pfcbl_category                                                   
  ),
  
  
  --#1: Calculations triggered by inserted parameters and new reporting
  calculation_parameter_triggers as (

      select 
        cpt.calculate_rsf_pfcbl_id,
        cpt.calculate_indicator_id,
        cpt.calculation_asof_date,
        cpt.formula_id
      from calculation_parameter_triggers_mat cpt
      -- because parameters to which it is subscribed may not also be calculations to which it is subscribed so re-filter on the calculation
      -- (we know its subscribed to the parameters because they've been inserted in the trigger's data)
      inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = cpt.calculate_rsf_pfcbl_id
                                                                 and sis.indicator_id = cpt.calculate_indicator_id
      where sis.formula_id = cpt.formula_id
        and sis.is_subscribed is true
  ),
  
  --#2: Calculations triggered by new reporting that listen to fx rate changes (regardless of any parameter changes)
  -- we are reporting for the first time this period
  -- and we monitor calculation formulas
  -- and those formulas are set to update according to fx rate changes
  -- and those formulas have, historically, used an fx rate in their calculation
  -- so recalculate them this period (even if no parameters have triggered it).
  calculation_fx_triggers as (
  
    select 
      rfs.rsf_pfcbl_id as calculate_rsf_pfcbl_id,
      indf.indicator_id as calculate_indicator_id,
      rfs.reporting_asof_date as calculation_asof_date,
      indf.formula_id
    from reporting_formula_subscriptions rfs
    inner join p_rsf.indicator_formulas indf on indf.formula_id = rfs.formula_id
    where indf.formula_fx_date = 'fx'
      and exists(select * from p_rsf.rsf_data_current_fx cfx
                 where cfx.rsf_pfcbl_id = rfs.rsf_pfcbl_id
                   and cfx.indicator_id = indf.indicator_id
                   and cfx.reporting_asof_date <= rfs.reporting_asof_date) -- it's listening to fx rates
	
  ), 
  
  --#3: Calculations triggered by a parameter that has _already_ been reported by a parent entity that was reported or calcualted already
  --ie, these calculations would be triggered if those parent reporting data were present in this cohort
  calculation_existing_parameter_triggers as (
  
    select 
      rfs.rsf_pfcbl_id as calculate_rsf_pfcbl_id,
      rfs.indicator_id as calculate_indicator_id,
      rfs.reporting_asof_date as calculation_asof_date,
      rfs.formula_id
    from reporting_formula_subscriptions rfs
    inner join p_rsf.indicator_formula_parameters ifp on ifp.formula_id = rfs.formula_id
                                                     and ifp.parameter_pfcbl_hierarchy = 'parent'
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rfs.rsf_pfcbl_id
    -- much faster
    inner join lateral (select ((array[0,ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id])[ifp.parameter_pfcbl_rank+1]) as rsf_pfcbl_id) as parent on true
    inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = parent.rsf_pfcbl_id
                                         and rdc.indicator_id = ifp.parameter_indicator_id
                                         and rdc.reporting_asof_date = rfs.reporting_asof_date                                
                                   
  ),
  
  --#4: Calculations triggered by the need for the system to verify/validate user-reported calculated values
  --ie, maybe no relevant parameters trigger this calculation; but the user has reported their own value for this calculation (correctly or not).
  --so trigger an evaluation so the system can check that the user has reported correctly.
  --but we don't want to trigger these for DELETES because the user hasn't actually reported in such an event.
  --and we don't want to trigger these for 'event_is_calculated_cohort' because this occurs when the system is reporting correct data, so we don't want to re-trigger the calculation!
  calculation_triggered_by_reporting as (
  
		select 
			mcd.rsf_pfcbl_id as calculate_rsf_pfcbl_id,
			mcd.indicator_id as calculate_indicator_id,
			mcd.reporting_asof_date as calculation_asof_date,
      sis.formula_id
		from _modified_data mcd
		inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = mcd.rsf_pfcbl_id 
                                                               and sis.indicator_id = mcd.indicator_id
		--inner join p_rsf.rsf_data rd on rd.data_id = mcd.data_id
		--inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
		where event_type <> 'delete' --true --(TG_OP <> 'DELETE') -- so it's an insert or an update: which can only have one cohort at a time inserted.  Deletes can be in bulk.

      -- NOT calculated insert into rsf_data (it's user reported calculation)
			-- if it's a calculated cohort then system is de-facto validating this
			-- we don't want to re-trigger the evaluation or the confirmation that it's validated.
		  and event_is_calculated_cohort is false
      and sis.is_subscribed = true
      and sis.formula_id is not null      
  ),
  
  calculations as (
  
    select * from calculation_parameter_triggers
    union all 
    select * from calculation_fx_triggers
    union all 
    select * from calculation_existing_parameter_triggers
    union all
    select * from calculation_triggered_by_reporting
  
  )
  insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
                                                     indicator_id,
                                                     calculation_asof_date)
  select x.calculate_rsf_pfcbl_id,x.calculate_indicator_id,x.calculation_asof_date
  from (  
    select 
      calc.calculate_rsf_pfcbl_id,
      calc.calculate_indicator_id,
      rpr.reporting_asof_date as calculation_asof_date
    from calculations calc
    inner join p_rsf.rsf_pfcbl_reporting rpr on rpr.rsf_pfcbl_id = calc.calculate_rsf_pfcbl_id
                                            and rpr.reporting_asof_date >= calc.calculation_asof_date
    
    except
    
    -- don't re-trigger a calculation if the calculation cohort has just inserted this
    select 
      md.rsf_pfcbl_id,
      md.indicator_id,
      md.reporting_asof_date
    from _modified_data md      
    where (event_type = 'insert') and (event_is_calculated_cohort is true)    
    
  ) x
  on conflict do nothing;    
  
  return;
  
end; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for function_get_indicator_calculation_rank
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."function_get_indicator_calculation_rank"("v_formula_id" int4, "v_indicator_id" int4, "v_formula_indicator_ids" _int4, "domsgs" bool, OUT "formula_recursive_rank" int4, OUT "formula_recursive_indicator_ids" _int4, OUT "formula_recursive_formula_ids" _int4);
CREATE FUNCTION "p_rsf"."function_get_indicator_calculation_rank"(IN "v_formula_id" int4, IN "v_indicator_id" int4, IN "v_formula_indicator_ids" _int4, IN "domsgs" bool=false, OUT "formula_recursive_rank" int4, OUT "formula_recursive_indicator_ids" _int4, OUT "formula_recursive_formula_ids" _int4)
  RETURNS "pg_catalog"."record" AS $BODY$
DECLARE counter int := 1;
declare msgs record;
--declare domsgs bool := true;
DECLARE v_formula_own_rank int;
DECLARE error_ids int[] := array[]::int[];
DECLARE error_entities int[] := array[]::int[];


BEGIN


    select pfcbl_rank
		into v_formula_own_rank
		from p_rsf.indicators ind
		inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
		where ind.indicator_id = v_indicator_id; 
		
		create temp table _recurse(recurse_id serial4,
		                           rank_depth int,
															 parent_indicator_id int,
															 parent_formula_id int,			
		                           calculation_indicator_id int,
															 calculation_formula_id int,
															 parameter_self_referential bool,
															 parameter_id int,
															 parameter_formula_id int,
															 primary key(recurse_id))
    on commit drop;
																		 
		
		create temp table _next(next_id serial4,
		                        rank_depth int,
		                        parent_indicator_id int,
														parent_formula_id int,			
		                        calculation_indicator_id int,
														calculation_formula_id int,
														parameter_self_referential bool,
														parameter_id int,
														parameter_formula_id int,
														primary key (next_id))
    on commit drop;

		insert into _recurse(rank_depth,
		                     parent_indicator_id,
												 parent_formula_id,
												 calculation_indicator_id,
												 calculation_formula_id,
												 parameter_self_referential,
												 parameter_id,
												 parameter_formula_id)
		select distinct
		  counter,                                   -- default start at 1.
			NULL::int as parent_indicator_id,
			NULL::int as parent_formula_id,
			v_indicator_id as calculation_indicator_id,
			v_formula_id as calculation_formula_id,
			pids.parameter_id = any(pindf.formula_indicator_ids) as parameter_self_referential,
			pids.parameter_id,                          -- non-calculated indicators may be present here
			pindf.formula_id as parameter_formula_id    -- null for non-calculated indicators
			
		from (select unnest(v_formula_indicator_ids) parameter_id) as pids
		left join p_rsf.indicator_formulas pindf on pindf.indicator_id = pids.parameter_id
		                                     	  and pindf.formula_id is distinct from v_formula_id
																						and pindf.indicator_id is distinct from v_indicator_id;  -- we're being calculated "now" so rank cannot 
																						                                                         -- be determined by self-referential formulas
																						                         -- may include self-referential parameters via v_formula_indicator_ids
		                                                                 -- and/or global parameters for non-global formulas
if (domsgs) then																																		 
		raise notice '
		
%',
(select indicator_name from p_rsf.indicators where indicator_id = v_indicator_id);
raise notice 'formula_indicator_ids=%',v_formula_indicator_ids;
		raise notice 'loop % size=%',counter,(select count(*) from _recurse);
end if;
	
		while counter < 20
		      AND exists(select * from _recurse rec
					           inner join p_rsf.indicator_formulas indf on indf.formula_id = rec.parameter_formula_id
										 left join lateral unnest(indf.formula_indicator_ids) as next_parameter_id on true
										 left join p_rsf.indicator_formulas pindf on pindf.indicator_id = next_parameter_id
										 left join p_rsf.indicators pind on pind.indicator_id = next_parameter_id
										 where rec.rank_depth = counter
										   and pindf.formula_id is distinct from v_formula_id
										 and rec.parameter_formula_id is distinct from pindf.formula_id
										 and rec.parameter_id is distinct from next_parameter_id										 																				
										 and (v_formula_own_rank = 0 OR pind.data_category is distinct from 'global')
										 
							)									 		
		loop		
		
			delete from _next;
			
			with next_params as (
			
				select distinct					
		      rec.calculation_indicator_id as parent_indicator_id,
					rec.calculation_formula_id as parent_formula_id,
					rec.parameter_id as calculation_indicator_id,
					rec.parameter_formula_id as calculation_formula_id,
					next_parameter_id = any(pindf.formula_indicator_ids) as parameter_self_referential,
					next_parameter_id as parameter_id,
					pindf.formula_id as parameter_formula_id
					
				from _recurse rec
				inner join p_rsf.indicator_formulas indf on indf.formula_id = rec.parameter_formula_id
				left join lateral unnest(indf.formula_indicator_ids) as next_parameter_id on true
				left join p_rsf.indicator_formulas pindf on pindf.indicator_id = next_parameter_id
				left join p_rsf.indicators pind on pind.indicator_id = next_parameter_id

				where rec.rank_depth = counter -- get the last-inserted not the counter+1 results
					-- self-referentiality can only exist where counter=1, which is loaded by default.
					and rec.parameter_formula_id is distinct from pindf.formula_id
					and pindf.formula_id is distinct from v_formula_id
					and rec.parameter_id is distinct from next_parameter_id				
					and (v_formula_own_rank = 0 OR pind.data_category is distinct from 'global')		
			   
			)
			insert into _next(parent_indicator_id,
												parent_formula_id,
												calculation_indicator_id,
												calculation_formula_id,
												parameter_self_referential,
												parameter_id,
												parameter_formula_id)
			select distinct 					
				np.parent_indicator_id,
				np.parent_formula_id,
				np.calculation_indicator_id,
				np.calculation_formula_id,
				np.parameter_self_referential,
				np.parameter_id,
				np.parameter_formula_id
			from next_params np;
			
		  counter := counter+1;			
			-- did this iteration learn something new?
      if exists(select 
			          parent_indicator_id,
								parent_formula_id,
								calculation_indicator_id,
								calculation_formula_id,
								parameter_self_referential,
								parameter_id,
								parameter_formula_id
								from _next
							  except
								select			
								parent_indicator_id,
								parent_formula_id,
								calculation_indicator_id,
								calculation_formula_id,
								parameter_self_referential,
								parameter_id,
								parameter_formula_id
								from _recurse rec)
		  then
			
			
			insert into _recurse(rank_depth,
													 parent_indicator_id,
													 parent_formula_id,
													 calculation_indicator_id,
													 calculation_formula_id,
													 parameter_self_referential,
													 parameter_id,
													 parameter_formula_id)
			select distinct 	
			  counter,				
				np.parent_indicator_id,
				np.parent_formula_id,
				np.calculation_indicator_id,
				np.calculation_formula_id,
				np.parameter_self_referential,
				np.parameter_id,
				np.parameter_formula_id
			from _next np;
			/*
			where not exists(select * from _recurse re
			                 where re.parent_indicator_id is not distinct from np.parent_indicator_id
											   and re.parent_formula_id is not distinct from np.parent_formula_id
												 
												 and re.calculation_indicator_id is not distinct from np.calculation_indicator_id
											   and re.calculation_formula_id is not distinct from np.calculation_formula_id
												 
												 and re.parameter_id is not distinct from np.parameter_id
												 and re.parameter_formula_id is not distinct from np.parameter_formula_id
												 and re.parameter_self_referential is not distinct from np.parameter_self_referential);*/
			--on conflict on constraint _rec_uni
			--do update set rank_depth=EXCLUDED.rank_depth;
			
			end if;


if (domsgs) then raise notice 'loop % size=%',counter,(select count(*) from _recurse); end if;
			
		end loop;

if (domsgs) then
FOR msgs IN 
SELECT
_r.rank_depth,
_r.parent_indicator_id,
_r.parent_formula_id,
_r.calculation_indicator_id,
_r.calculation_formula_id,
_r.parameter_self_referential,
cind.indicator_name as cname,
_r.parameter_id,
_r.parameter_formula_id,
pind.indicator_name as pname
FROM _recurse _r 
inner join p_rsf.indicators cind on cind.indicator_id = _r.calculation_indicator_id
left join p_rsf.indicators pind on pind.indicator_id = _r.parameter_id 
--where exists(select * from p_rsf.indicator_formulas indf where indf.indicator_id = ind.indicator_id)
order by _r.rank_depth,
_r.parent_indicator_id nulls first,_r.parent_formula_id nulls first,
_r.calculation_indicator_id,_r.calculation_formula_id,_r.parameter_id nulls first,_r.parameter_formula_id nulls first
LOOP
 RAISE NOTICE '% % xf% c% cf% sr=% cn=% p% pf% pn=%',
 concat(msgs.rank_depth,'/',counter),
 msgs.parent_indicator_id,
 msgs.parent_formula_id,
 msgs.calculation_indicator_id,
 msgs.calculation_formula_id,
 msgs.parameter_self_referential,
 msgs.cname,
 msgs.parameter_id,
 msgs.parameter_formula_id,
 msgs.pname;
END LOOP;
end if;

if (counter >= 20) 
then

raise exception 'Recursion depth of 20 exceeded for nested calclations';

end if;



    -- Cricular references are somewhat dependent on what an RSF entitity's formula subscriptions are.
		-- ie, if a facility defines an indicator as manually calculated and/or reported then there is not any actual circular refernece 
		-- because the facility will never calculate it.
		-- The recursive search for prerquisite indicatoes does NOT consider which formula_ids are actually used, but only what indicators are used and ALL
		-- their potential formulas. This is to make indicator ranks and triggering parameters both more simple and more static (we don't want to recalcualte
		-- the indicator ranks each time a RSF program changes or updates its subscriptions).  This could potentially trigger a lot of recalculations following
		-- a subscription change.
		-- Instead, we call this function within indicator subscriptions to raise an exception if a new subscription could cause a circular reference.
		-- This makes the indicators more static and efficient over all.  The down side is that an indicaitor that has a potential circular reference 
		-- within its prerequisite chain will have a higher calculation rank than it (might) otherwise.  This is a much less significant efficiency loss.
    if (exists(select * from _recurse
		           where parameter_id = v_indicator_id
							   and rank_depth > 1))
	  then

			 select array_agg(distinct _r.calculation_formula_id)
			 into error_ids
			 from _recurse _r
			 where _r.parameter_id = v_indicator_id
			   and _r.rank_depth > 1;
				 
			 error_ids := array_remove(error_ids,v_formula_id);
			 select array_agg(distinct eids.rsf_pfcbl_id)
			 into error_entities
			 from (select fis.rsf_pfcbl_id
						from p_rsf.view_rsf_program_facility_indicator_subscriptions fis 
						where fis.formula_id = any(error_ids || array[v_formula_id])
							and (fis.is_subscribed = true OR fis.is_auto_subscribed = true)
						group by fis.rsf_pfcbl_id
						having 
							v_formula_id = any(array_agg(distinct fis.formula_id))
							and array_agg(distinct fis.formula_id) && error_ids
							) eids;

			 if (NOT (error_entities is NULL or cardinality(error_entities) = 0))
			 then 
					raise exception 'Circular reference formula error between formula_ids %: 
					% (%) FORMULA: % 
					and 
					%
					for : %',
					uniq(sort((error_ids || array[v_formula_id]))),
					(select ind.indicator_name from p_rsf.indicators ind where ind.indicator_id = v_indicator_id),
					v_indicator_id,
					(select indf.formula from p_rsf.indicator_formulas indf where indf.formula_id = v_formula_id),
					(select array_to_string(array_agg(distinct concat(ind.indicator_name,' FORMULA: ',indf.formula)),' 
					')
					 from p_rsf.indicators ind 
					 inner join p_rsf.indicator_formulas indf on indf.indicator_id = ind.indicator_id
					 where indf.formula_id = any(error_ids)),
					 (select array_to_string(array_agg(distinct sn.sys_name),'
					  and ')
						from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
						where sn.rsf_pfcbl_id = any(error_entities));
			 end if;
		end if;
		
	error_ids := array[]::int[];
	error_entities := array[]::int[];
	
	
		
 -- counter can be GREATER THAN max(rank_depth) because recursion can stop when
 -- (a) we find no more calculated parameters to iterate over
 -- (b) we've learned nothing new
 -- If we learn nothing new, then rank_depth won't increase, but counter WILL increase and indicate that the additional
 -- recurions was necessary because the previous input thought that we COULD learn something new and prompted the additional iteration
 -- This is most likely to occur when self-referential formulas are in the calculation chain or when multiple inputs use similar parameters
 -- and querying those parameters learnes nothing new, but the additional iteration is required to consider those inputs' calculation chains.
 -- NOT THIS: formula_recursive_rank := (select max(rank_depth) from _recurse);
 formula_recursive_rank := counter;
 formula_recursive_indicator_ids := (select array_agg(parameter_id) from _recurse where parameter_id is not null);
 formula_recursive_indicator_ids := uniq(sort(formula_recursive_indicator_ids));
 
 formula_recursive_formula_ids := (select array_agg(calculation_formula_id) from _recurse where calculation_formula_id is not null);
 
 if (NOT v_indicator_id = any(v_formula_indicator_ids)) 
 then
	formula_recursive_formula_ids := array_remove(formula_recursive_formula_ids,v_formula_id);
 end if;
 formula_recursive_formula_ids := uniq(sort(formula_recursive_formula_ids));
 
select array_agg(distinct indf.formula_id)
into error_ids	
from p_rsf.indicator_formulas indf 
inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id	
where indf.formula_id <> v_formula_id
  and indf.indicator_id <> v_indicator_id -- we don't want it to pick-out a differently ranked formula for itself.
	and indf.formula_id = any(formula_recursive_formula_ids)
	and indf.formula_calculation_rank >= formula_recursive_rank
	and (v_formula_own_rank = 0 OR ind.data_category <> 'global');
	
 if (NOT (error_ids is NULL or cardinality(error_ids) = 0))
 then
	raise exception 'Circular formula references: 
	indicator % formula% depends on same calculation rank % as prerequisite parameter:
	%
	Try re-writing the formula to avoid the dependency?',
	(select ind.indicator_name from p_rsf.indicators ind where ind.indicator_id = v_indicator_id),
	v_formula_id,
	formula_recursive_rank,
	(select array_to_string(array_agg(distinct concat(ind.indicator_name,' rank=',indf.formula_calculation_rank,' formula',indf.formula_id)),' and
	')
	 from p_rsf.indicator_formulas indf
	 inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
	 where indf.formula_id = any(error_ids)
	   and indf.formula_calculation_rank >= formula_recursive_rank);
		 
 end if;



 drop table _next;
 drop table _recurse;

 return;


END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for function_rsf_data_current_update
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."function_rsf_data_current_update"();
CREATE FUNCTION "p_rsf"."function_rsf_data_current_update"()
  RETURNS "pg_catalog"."void" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
begin
  
	if (not exists(select * from _temp_current_data))
	then 
		return;
	end if;
	
	/********************************************
	rsf_data_current
	********************************************/

  raise info ' - function_rsf_data_current_update(%)',
	(select count(*) from _temp_current_data); msg_time := clock_timestamp();
	
--raise info 'function_rsf_data_current_update export table data: %',(select json_agg(tab) FROM (SELECT * from _temp_current_data) tab);
--msg_time := clock_timestamp();
-- basically acts as a before insert trigger on rsf_data_current								 
-- insert LCU values first so that they become avaialble for currency data types in the subsequent upsert.
  if exists(select * from _temp_current_data 
	          where indicator_sys_category in ('entity_currency_unit',        -- defined currency unit
		                                         'entity_local_currency_unit'))
	then
	
	  msg_time := clock_timestamp(); 
		
		insert into p_rsf.rsf_data_current(data_id,
																			 rsf_pfcbl_id,
																			 indicator_id,
																			 reporting_asof_date,
																			 data_value,
																			 data_unit,
																			 data_unit_data_id)
		select 
			ird.data_id,
			ird.rsf_pfcbl_id,
			ird.indicator_id,
			ird.reporting_asof_date,
			ird.data_value,
			NULL::text as data_unit,       -- is_data_unit indicators do not have units, by definition: data_type = text
			NULL::int as data_unit_data_id -- is_data_unit indicators do not have units, by definition: data_type = text
		from _temp_current_data ird
		where ird.indicator_sys_category in ('entity_currency_unit',        -- defined currency unit
		                                     'entity_local_currency_unit')  -- inherited currency unit
      -- NULL currency units have no meaning, so don't add them if triggered for some reason																
			and ird.data_value is NOT NULL
		on conflict(rsf_pfcbl_id,indicator_id,reporting_asof_date)
		do update
		set
			data_id = EXCLUDED.data_id,
			data_value = EXCLUDED.data_value,
			data_unit = EXCLUDED.data_unit,
			data_unit_data_id = EXCLUDED.data_unit_data_id;
	
	  create temp table _temp_lcu(lcu_unit_data_id int,
		                            for_rsf_pfcbl_id int,
																reporting_asof_date date,
																data_unit_value text,
																data_id_pfcbl_rank int,
																is_defined_lcu bool)
    on commit drop;
		
		
		with affirm_lcu as MATERIALIZED (
				select 
					ird.rsf_pfcbl_id
				from _temp_current_data ird
				where ird.indicator_sys_category = 'entity_currency_unit'
				
				union 
				
				-- not including a check on created_in_reporting_asof_date on the child: because parent could update its LCU value historically
				-- and it should cascade forward to the subsequently created child entity.
        select 
          ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
        from _temp_current_data ird
        inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = ird.rsf_pfcbl_id
                                                         and ft.pfcbl_hierarchy <> 'parent' -- ie, is self OR child
        where ird.indicator_sys_category = 'entity_local_currency_unit'
        /*
				select 
					fam.child_rsf_pfcbl_id as rsf_pfcbl_id
				from _temp_current_data ird
				inner join p_rsf.rsf_pfcbl_id_family fam on fam.parent_rsf_pfcbl_id = ird.rsf_pfcbl_id		
				where ird.indicator_sys_category = 'entity_local_currency_unit'
        */
		)
		insert into _temp_lcu(lcu_unit_data_id,
													for_rsf_pfcbl_id,
													reporting_asof_date,
													data_unit_value,
													data_id_pfcbl_rank,
													is_defined_lcu) 
		select distinct on (cuad.for_rsf_pfcbl_id,cuad.reporting_asof_date)
			cuad.lcu_unit_data_id,
			cuad.for_rsf_pfcbl_id,
			cuad.reporting_asof_date,
			cuad.data_unit_value,
			cuad.data_id_pfcbl_rank,
			cuad.is_defined_lcu
		from affirm_lcu alcu
		inner join p_rsf.view_rsf_pfcbl_currency_units_asof_date cuad on cuad.for_rsf_pfcbl_id = alcu.rsf_pfcbl_id
		--a complete re-check including into the future because historic updates can affect future and historic deletes affect
		--entities created into the future that are using these older LCU definitions.
		--where cuad.reporting_asof_date <= alcu.reporting_asof_date
		order by 
			cuad.for_rsf_pfcbl_id,
			cuad.reporting_asof_date,
			is_defined_lcu desc,          -- defined takes priority
			cuad.data_id_pfcbl_rank desc; -- lower-level parents take priority																
/*
	raise info 'TESTING contents of _temp_lcu entity_local_currency_unit: %',		
	(select left(json_agg(row_to_json(js))::text,900)
	 from (	 
	 select * from _temp_lcu	 
	 --select * from p_rsf.view_rsf_pfcbl_currency_units_asof_date where for_rsf_pfcbl_id = 108839 
	 ) as js);
*/    
		analyze _temp_lcu;
		alter table _temp_lcu add primary key(for_rsf_pfcbl_id,reporting_asof_date);
						

	


	  delete from p_rsf.rsf_data_current_lcu lcu
		where exists(select * from _temp_lcu tu 
			           where tu.for_rsf_pfcbl_id = lcu.for_rsf_pfcbl_id)
			and not exists(select * from _temp_lcu tu
		                 where tu.for_rsf_pfcbl_id = lcu.for_rsf_pfcbl_id
											 and tu.reporting_asof_date = lcu.reporting_asof_date);
    
		insert into p_rsf.rsf_data_current_lcu(lcu_unit_data_id,
																					 for_rsf_pfcbl_id,
																					 reporting_asof_date,
																					 data_unit_value,
																					 data_id_pfcbl_rank,
																					 is_defined_lcu)
		select 
			tu.lcu_unit_data_id,
			tu.for_rsf_pfcbl_id,
			tu.reporting_asof_date,
			tu.data_unit_value,
			tu.data_id_pfcbl_rank,
			tu.is_defined_lcu
		from _temp_lcu tu
		where not exists(select * from p_rsf.rsf_data_current_lcu lcu
										 where lcu.for_rsf_pfcbl_id = tu.for_rsf_pfcbl_id
										   and lcu.reporting_asof_date = tu.reporting_asof_date
											 and lcu.lcu_unit_data_id = tu.lcu_unit_data_id
											 and lcu.data_unit_value = tu.data_unit_value)
		on conflict(for_rsf_pfcbl_id,reporting_asof_date)
		do update
		set lcu_unit_data_id = EXCLUDED.lcu_unit_data_id,
				data_unit_value = EXCLUDED.data_unit_value,
				data_id_pfcbl_rank = EXCLUDED.data_id_pfcbl_rank,
				is_defined_lcu = EXCLUDED.is_defined_lcu;
		
		drop table _temp_lcu;
		
      raise info ' - function_rsf_data_current_update: completed local/currency_units: in %',
      (select clock_timestamp()-msg_time); msg_time := clock_timestamp();

	end if;
/*
	
	raise info 'TESTING contents of _temp_current_data entity_local_currency_unit: %',		
	(select left(json_agg(row_to_json(js))::text,900)
	 from (	 
	 
	 ) as js);
*/


--select * from p_rsf.indicators where indicator_id = 157434
		insert into p_rsf.rsf_data_current(data_id,
																			 rsf_pfcbl_id,
																			 indicator_id,
																			 reporting_asof_date,
																			 data_value,
																			 data_unit,
																			 data_unit_data_id)
		select 
			ird.data_id,
			ird.rsf_pfcbl_id,
			ird.indicator_id,
			ird.reporting_asof_date,

			-- ensure reported values are always in alphabetic order to reliably query values and know to multiply/divide results
			case when p_rsf.fx_currency_ratio_has_alphabetic_order(fx.data_unit_ratio) = false
		       then (1/(ird.data_value::numeric))::text
				 else ird.data_value
			end as data_value,

			case when p_rsf.fx_currency_ratio_has_alphabetic_order(fx.data_unit_ratio) = false
		     then p_rsf.fx_currency_ratio_in_alphabetic_order(fx.data_unit_ratio)
				 else fx.data_unit_ratio
			end as data_unit,
			
			case when fx.data_unit_ratio ~ lcu.data_unit_value 			
					 then lcu.lcu_unit_data_id 
					 else NULL
			end as data_unit_data_id

		from _temp_current_data ird
		left join lateral (select
												 lcu.data_unit_value,
												 lcu.lcu_unit_data_id
											 from p_rsf.rsf_data_current_lcu lcu 
											 where lcu.for_rsf_pfcbl_id = ird.rsf_pfcbl_id
											   and lcu.reporting_asof_date <= ird.reporting_asof_date
											 order by lcu.reporting_asof_date desc
											 limit 1) as lcu on true											 
    left join lateral (select case												 
												 when ird.data_unit ~ 'LCU' -- User submitted proper data unit, eg EUR/LCU (or inverse LCU/EUR possibly?)
													and lcu.lcu_unit_data_id is not null -- And I have a defined LC unit
												 then regexp_replace(ird.data_unit,'LCU',lcu.data_unit_value)
												 
												 when (ird.data_unit ~ '/') = false -- User submitted impropper data unit, eg EUR
													and lcu.lcu_unit_data_id is not null -- And I have a defined LC unit, then assume they meant EUR/LCU
												 then ird.data_unit || '/' || lcu.data_unit_value
												 
												 else ird.data_unit 
											 end as data_unit_ratio
											) as fx on true 
    where ird.data_type = 'currency_ratio' -- currency_ratios only exist (currently) at facility level (values/sources defined by the facility) or
		                                       -- global, sourced from IFC fx database
  													               -- note indicator check for USD/LCU: currency_ratio_data_type_LCU_must_be_denominator
		
		on conflict(rsf_pfcbl_id,indicator_id,reporting_asof_date) -- rsf_pfcbl_id,indicator_id,reporting_asof_date
		do update
		set
			data_id = EXCLUDED.data_id,
			data_value = EXCLUDED.data_value,
			data_unit = EXCLUDED.data_unit,
			data_unit_data_id = EXCLUDED.data_unit_data_id;


   raise info ' - function_rsf_data_current_update: current currency ratios in %',
   (select clock_timestamp()-msg_time); msg_time := clock_timestamp();

		insert into p_rsf.rsf_data_current(data_id,
																			 rsf_pfcbl_id,
																			 indicator_id,
																			 reporting_asof_date,
																			 data_value,
																			 data_unit,
																			 data_unit_data_id)
		select 
			ird.data_id,
			ird.rsf_pfcbl_id,
			ird.indicator_id,
			ird.reporting_asof_date,

			-- ensure reported values are always in alphabetic order to reliably query values and know to multiply/divide results
			ird.data_value,
			
			case when ird.data_unit = 'LCU'
			     then lcu.data_unit_value
					 else ird.data_unit
		  end as data_unit,
			
			case when ird.data_unit = 'LCU' or ird.data_unit = lcu.data_unit_value
					 then lcu.lcu_unit_data_id 
					 else NULL
			end as data_unit_data_id
						
		from _temp_current_data ird
		--inner join p_rsf.indicators ind on ind.indicator_id = ird.indicator_id
		left join lateral (select
												 lcu.data_unit_value,
												 lcu.lcu_unit_data_id
											 from p_rsf.rsf_data_current_lcu lcu 
											 where lcu.for_rsf_pfcbl_id = ird.rsf_pfcbl_id
											   and lcu.reporting_asof_date <= ird.reporting_asof_date
											 order by lcu.reporting_asof_date desc
											 limit 1) as lcu on true
    where ird.data_type = 'currency' 
		on conflict(rsf_pfcbl_id,indicator_id,reporting_asof_date) -- rsf_pfcbl_id,indicator_id,reporting_asof_date
		do update
		set
			data_id = EXCLUDED.data_id,
			data_value = EXCLUDED.data_value,
			data_unit = EXCLUDED.data_unit,
			data_unit_data_id = EXCLUDED.data_unit_data_id;

	raise notice ' - function_rsf_data_current_update: current currencies: %',
  (clock_timestamp()-msg_time); msg_time := clock_timestamp();



		insert into p_rsf.rsf_data_current(data_id,
																			 rsf_pfcbl_id,
																			 indicator_id,
																			 reporting_asof_date,
																			 data_value,
																			 data_unit,
																			 data_unit_data_id)
		select 
			ird.data_id,
			ird.rsf_pfcbl_id,
			ird.indicator_id,
			ird.reporting_asof_date,
			ird.data_value,
			ird.data_unit,
			NULL::int as data_unit_data_id
			
		from _temp_current_data ird
		--inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = ird.reporting_cohort_id
		--inner join p_rsf.indicators ind on ind.indicator_id = ird.indicator_id
		where ird.data_type not in ('currency','currency_ratio')
		  and coalesce(ird.indicator_sys_category not in ('entity_currency_unit',        
		                                                  'entity_local_currency_unit'),true)
  on conflict(rsf_pfcbl_id,indicator_id,reporting_asof_date) -- rsf_pfcbl_id,indicator_id,reporting_asof_date
	do update
	set
		data_id = EXCLUDED.data_id,
		data_value = EXCLUDED.data_value,
		data_unit = EXCLUDED.data_unit,
		data_unit_data_id = EXCLUDED.data_unit_data_id; 

	raise notice ' - function_rsf_data_current_update: current data: %',
  (clock_timestamp()-msg_time); msg_time := clock_timestamp();


/*
  --https://stackoverflow.com/questions/24006291/postgresql-return-result-set-as-json-array
raise info 'inserted data %',
(SELECT json_agg(t) FROM (select * from _temp_current_data) as t);


--https://stackoverflow.com/questions/24006291/postgresql-return-result-set-as-json-array
raise info 'current data %',
(SELECT json_agg(t) FROM (select * from p_rsf.rsf_data_current rdc
where rdc.rsf_pfcbl_id = 56341 and rdc.indicator_id = 157438) as t);
*/


 return;
 
END $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for function_rsf_loan_issuance_series
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."function_rsf_loan_issuance_series"("input_rsf_pfcbl_id" int4, "input_id_value" text, "input_id_indicator_id" int4);
CREATE FUNCTION "p_rsf"."function_rsf_loan_issuance_series"("input_rsf_pfcbl_id" int4, "input_id_value" text, "input_id_indicator_id" int4)
  RETURNS TABLE("parent_rsf_pfcbl_id" int4, "matched_rsf_pfcbl_id" int4, "matched_data_id" int4, "series_id" text) AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
begin
  return query
  select
		loans.rsf_borrower_id as parent_rsf_pfcbl_id,
		loans.rsf_pfcbl_id as matched_rsf_pfcbl_id,
		idvalues.data_id as matched_data_id,
		loans.rsf_borrower_id || ':' || sibling_id as series_id
	from p_rsf.rsf_pfcbl_ids ids
	inner join p_rsf.rsf_pfcbl_ids loans on loans.rsf_borrower_id = ids.rsf_borrower_id
	inner join lateral (select
												rdc.data_id,
												rdc.data_value
	                    from p_rsf.rsf_data_current rdc 
											where rdc.rsf_pfcbl_id = loans.rsf_pfcbl_id
											  and rdc.indicator_id = input_id_indicator_id
											order by rdc.reporting_asof_date desc -- not relative to a reporting date, but an absolute determination 
											limit 1) as idvalues on true
	inner join lateral p_rsf.rsf_data_id_normalized(input_id => idvalues.data_value) as sibling_id	on true
	--inner join p_rsf.rsf_data_id_normalized(input_id => input_id_value) as own_id on own_id.id_normalized = sibling_id

	
	inner join p_rsf.rsf_data_id_normalized(input_id => input_id_value) as own_id on (own_id.id_normalized = sibling_id
																																									  or
																																										own_id.id_normalized ~ sibling_id
																																										or 
																																										sibling_id = own_id.id_normalized)
  																																										
	where ids.rsf_pfcbl_id = input_rsf_pfcbl_id
    and ids.pfcbl_category = 'loan'
		and loans.pfcbl_category = 'loan'
		and loans.rsf_pfcbl_id <> input_rsf_pfcbl_id;
end; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100
  ROWS 1000;

-- ----------------------------
-- Function structure for fx_currency_ratio_has_alphabetic_order
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."fx_currency_ratio_has_alphabetic_order"("v_currency_ratio" text);
CREATE FUNCTION "p_rsf"."fx_currency_ratio_has_alphabetic_order"("v_currency_ratio" text)
  RETURNS "pg_catalog"."bool" AS $BODY$
BEGIN
	
	return (substring(v_currency_ratio,'^([A-Z]{3})/[A-Z]{3}$') <= substring(v_currency_ratio,'^[A-Z]{3}/([A-Z]{3})$'))::bool;

END;
$BODY$
  LANGUAGE plpgsql IMMUTABLE
  COST 100;

-- ----------------------------
-- Function structure for fx_currency_ratio_has_numerator
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."fx_currency_ratio_has_numerator"("v_currency_ratio" text, "v_test_numerator" text);
CREATE FUNCTION "p_rsf"."fx_currency_ratio_has_numerator"("v_currency_ratio" text, "v_test_numerator" text)
  RETURNS "pg_catalog"."bool" AS $BODY$
BEGIN
	
	return upper(substring(v_currency_ratio,'^([A-Z]{3})/[A-Z]{3}$')) = upper(v_test_numerator);

END;
$BODY$
  LANGUAGE plpgsql IMMUTABLE
  COST 100;

-- ----------------------------
-- Function structure for fx_currency_ratio_in_alphabetic_order
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."fx_currency_ratio_in_alphabetic_order"("v_currency_ratio" text);
CREATE FUNCTION "p_rsf"."fx_currency_ratio_in_alphabetic_order"("v_currency_ratio" text)
  RETURNS "pg_catalog"."text" AS $BODY$
BEGIN
	
	return case when p_rsf.fx_currency_ratio_has_alphabetic_order(v_currency_ratio) = true
	            then v_currency_ratio
							else p_rsf.fx_currency_ratio_inverse(v_currency_ratio) end;

END;
$BODY$
  LANGUAGE plpgsql IMMUTABLE
  COST 100;

-- ----------------------------
-- Function structure for fx_currency_ratio_inverse
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."fx_currency_ratio_inverse"("v_currency_ratio" text);
CREATE FUNCTION "p_rsf"."fx_currency_ratio_inverse"("v_currency_ratio" text)
  RETURNS "pg_catalog"."text" AS $BODY$
BEGIN
	
	return substring(v_currency_ratio,'^[A-Z]{3}/([A-Z]{3})$') || '/' || substring(v_currency_ratio,'^([A-Z]{3})/[A-Z]{3}$');

END;
$BODY$
  LANGUAGE plpgsql IMMUTABLE
  COST 100;

-- ----------------------------
-- Function structure for fx_pfcbl_convert_currency
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."fx_pfcbl_convert_currency"("input_rsf_pfcbl_id" int4, "input_from_currency" text, "input_to_currency" text, "input_exchange_rate_date" date, "input_force_global_fx_rate" bool, OUT "exchange_rate_date" date, OUT "fx_indicator_id" int4, OUT "fx_pfcbl_category" text, OUT "exchange_rate_data_id" int4, OUT "currency_ratio" text, OUT "exchange_rate" numeric, OUT "is_invalidated" bool, OUT "is_unreported" bool);
CREATE FUNCTION "p_rsf"."fx_pfcbl_convert_currency"(IN "input_rsf_pfcbl_id" int4, IN "input_from_currency" text, IN "input_to_currency" text, IN "input_exchange_rate_date" date, IN "input_force_global_fx_rate" bool=false, OUT "exchange_rate_date" date, OUT "fx_indicator_id" int4, OUT "fx_pfcbl_category" text, OUT "exchange_rate_data_id" int4, OUT "currency_ratio" text, OUT "exchange_rate" numeric, OUT "is_invalidated" bool, OUT "is_unreported" bool)
  RETURNS "pg_catalog"."record" AS $BODY$
DECLARE v_currency_ratio text;
DECLARE v_inverse bool default NULL;
DECLARE v_fx_rsf_pfcbl_id int;
DECLARE v_fx_data_unit_currency_ratio text;
BEGIN

  if (input_from_currency is NULL OR
	    input_to_currency is NULL) 
	then
		return;
	end if;

  -- EUR/USD 1.20 is FROM EURO TO USD	and means MULTIPLY (100) EURO by 1.2 to get 120 USD.
	-- See https://www.thebalancemoney.com/how-to-read-and-calculate-exchange-rates-1978919 on reading fx rates
	-- If the fx rate we've saved in the database is USD/EUR, then we need to invert the saved value to EUR/USD
	-- to be able to reliably multiply 120 USD by .8333 to get 100 EUR
	currency_ratio := input_from_currency || '/' || input_to_currency;
	
	if input_to_currency < input_from_currency 
		then 
			v_currency_ratio := input_to_currency || '/' || input_from_currency;
		else	
		  -- from/to is in alphabetic order
			v_currency_ratio := input_from_currency || '/' || input_to_currency;
	end if;
	
	
	-- unary, eg, USD/USD requested will always be 1
	if (input_from_currency = input_to_currency) then	
		
		select
			input_exchange_rate_date as exchange_rate_date,
			NULL::int as fx_indicator_id,
			'global' as fx_pfcbl_category,
			NULL::int as exchange_rate_data_id,
			input_from_currency || '/' || input_to_currency as v_fx_data_unit_currency_ratio,
			1::numeric as exchange_rate,
			0::int as fx_rsf_pfcbl_id,
			false as is_invalidated,
			false as is_unreported			
		into 
			exchange_rate_date,
			fx_indicator_id,
			fx_pfcbl_category,
			exchange_rate_data_id,
			v_fx_data_unit_currency_ratio,
			exchange_rate,
			v_fx_rsf_pfcbl_id,
			is_invalidated,
			is_unreported;
			
		v_inverse := false;
	else 	

		
	
	  -- Modified in Jan 2024: Replaced joins with lateral joins for last-reported value because the assumption that 
		-- fx rates would change and get reported each QR was a bad assumption violated by pegged currencies whose fx
		-- rate was static.  And therefore, we need to return last-reported values and not value as-of the requested date.
		-- This raises the uncertainty for all other floating fx rates that will change constantly and knowing whether or not 
		-- they have properly been computed as of the given request date.  Hence the addition of checks on whether the entity 
		-- reported or whether the calculation is pending.
	  if (input_force_global_fx_rate = true) 
		then
			select 
					rdc.reporting_asof_date as exchange_rate_date,
					ind.indicator_id as fx_indicator_id,
					ind.data_category as fx_pfcbl_category,
					rdc.data_id as exchange_rate_data_id,
				  --input_to_currency || '/' || input_from_currency as currency_ratio,
          rdc.data_unit as v_fx_data_unit_currency_ratio,
					rdc.data_value::numeric as exchange_rate,					
					rdc.rsf_pfcbl_id as fx_rsf_pfcbl_id
					
				into 
					exchange_rate_date,
					fx_indicator_id,
					fx_pfcbl_category,
					exchange_rate_data_id,
					v_fx_data_unit_currency_ratio,
					exchange_rate,
					v_fx_rsf_pfcbl_id
				from p_rsf.indicators ind
				left join lateral (select
				                      rdc.data_id,
															rdc.data_value,
															rdc.data_unit,
															rdc.rsf_pfcbl_id,
															rdc.reporting_asof_date
													  from p_rsf.rsf_data_current rdc 
														where rdc.rsf_pfcbl_id = 0 
				                      and rdc.indicator_id = ind.indicator_id
															and rdc.reporting_asof_date <= input_exchange_rate_date::date
													  order by 
														  rdc.reporting_asof_date desc
															limit 1) as rdc on true
				where ind.data_category = 'global'
				  and ind.data_unit = v_currency_ratio;
					
		-- Else don't force global, rather lookup locally-defined FX rates (ie, those defined and reported at facility level, where applicable!)			
		ELSE
    select 
				rdc.reporting_asof_date as exchange_rate_date,
				ind.indicator_id as fx_indicator_id,
				--fam.parent_pfcbl_category as fx_pfcbl_category,
        ft.to_pfcbl_category as fx_pfcbl_category,
				rdc.data_id as exchange_rate_data_id,
				rdc.data_unit as v_fx_data_unit_currency_ratio,
			  --input_to_currency || '/' || input_from_currency as currency_ratio,
				rdc.data_value::numeric as exchange_rate,
        ft.to_family_rsf_pfcbl_id
				--fam.parent_rsf_pfcbl_id
				
			into 
				exchange_rate_date,
				fx_indicator_id,
				fx_pfcbl_category,
				exchange_rate_data_id,
				v_fx_data_unit_currency_ratio,
				exchange_rate,
				v_fx_rsf_pfcbl_id
        
			from p_rsf.view_rsf_pfcbl_id_family_tree ft
			inner join p_rsf.indicators ind on ind.data_category = ft.to_pfcbl_category
			                               and ind.data_type = 'currency_ratio'
			left join lateral (select data_unit_value
			                   from p_rsf.rsf_data_current_lcu lcu
												 where lcu.for_rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
			                     and lcu.reporting_asof_date <= input_exchange_rate_date::date
												 order by lcu.reporting_asof_date desc
												 limit 1) lcu on true

      left join lateral (select
				                      rdc.data_id,
															rdc.data_value,
															rdc.data_unit,
															rdc.reporting_asof_date
													  from p_rsf.rsf_data_current rdc 
														where rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
				                      and rdc.indicator_id = ind.indicator_id
															and rdc.reporting_asof_date <= input_exchange_rate_date::date
													  order by 
														  rdc.reporting_asof_date desc
															limit 1) as rdc on true

			where ft.from_rsf_pfcbl_id = input_rsf_pfcbl_id 
        and ft.pfcbl_hierarchy <> 'child' -- ie, self or parent.
			  -- check enforces currency_ratio_data_type_LCU_must_be_denominator LCU in denominator if it's an LCU indicator
				and (ind.data_unit = v_currency_ratio OR
				     coalesce(rdc.data_unit=v_currency_ratio,false) OR
			       p_rsf.fx_currency_ratio_in_alphabetic_order(regexp_replace(ind.data_unit,'LCU',lcu.data_unit_value)) = v_currency_ratio)
						 
			  -- 2024-11-15: programs not subscribed to USD_LCU fx indicator were querying it.
			  -- exists are much faster than joining the subscriptions view
			  and ((ind.data_category = 'global' and ft.to_family_rsf_pfcbl_id = 0)
				     or 
             (
				       -- have I ever reported on it (not just subscribed to it!)
							 -- if it's an empty/mistaken subscription then don't return it and then tell us it's missing a data_id
							 -- which will given the system calculation an fx error.
						   exists(select * from p_rsf.rsf_data_current rdc
					            where rdc.indicator_id = ind.indicator_id
											  and rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id)
							 and 
               
               exists(select * from p_rsf.view_rsf_setup_indicator_subscriptions sis
                      where sis.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                        and sis.indicator_id = ind.indicator_id
                        and sis.is_subscribed is true)
						   -- is the requested entity subscribed to the indicator?
						   -- fx indicators only at facility or global levels
							 -- if facility is UN-subscribed, will return false and return global-level
							 -- if facility is empty, will return subscription at program level (true or false)
							 -- if program is empty, will return NULL is true (false)
               /*
						   (select pfi.is_subscribed
							  from p_rsf.rsf_pfcbl_ids ids 
								inner join p_rsf.rsf_program_facility_indicators pfi on pfi.rsf_program_id = ids.rsf_program_id
								where ids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
								  and pfi.indicator_id = ind.indicator_id
                  and (pfi.rsf_facility_id is NULL or pfi.rsf_facility_id is not distinct from ft.to_family_rsf_pfcbl_id)
								order by pfi.rsf_facility_id is NOT NULL DESC
								limit 1) is true */
					   )
			   ) 
												 
			order by 
				
				rdc.data_id is not null desc, -- if uses a facility-level indicator but no data submitted, then default to global.
				ft.to_pfcbl_rank desc,   -- from self closest to global last
				rdc.reporting_asof_date desc, -- if facility uploaded multiple (eg, LCU/USD where LCU=EUR and EUR/USD) then most recent reporting
				ind.data_unit ~ 'LCU' desc    -- facility *could* define LCU/USD and USD/XOF fx rate columns and its LCU is XOF, meaning
				                              -- two facility-level indicators are (presumably) capturing the same fx rate at the same time.
																	    -- hopefully client is reporting these identically...but prefer the defined USD/XOF over the implied LCU/USD
			limit 1;	
    /*
			--raise notice 'YES';
			select 
				rdc.reporting_asof_date as exchange_rate_date,
				ind.indicator_id as fx_indicator_id,
				fam.parent_pfcbl_category as fx_pfcbl_category,
				rdc.data_id as exchange_rate_data_id,
				rdc.data_unit as v_fx_data_unit_currency_ratio,
			  --input_to_currency || '/' || input_from_currency as currency_ratio,
				rdc.data_value::numeric as exchange_rate,
				fam.parent_rsf_pfcbl_id
				
			into 
				exchange_rate_date,
				fx_indicator_id,
				fx_pfcbl_category,
				exchange_rate_data_id,
				v_fx_data_unit_currency_ratio,
				exchange_rate,
				v_fx_rsf_pfcbl_id
        
			from p_rsf.rsf_pfcbl_id_family fam
			inner join p_rsf.indicators ind on ind.data_category = fam.parent_pfcbl_category
			
			left join lateral (select data_unit_value
			                   from p_rsf.rsf_data_current_lcu lcu
												 where lcu.for_rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
			                     and lcu.reporting_asof_date <= input_exchange_rate_date::date
												 order by lcu.reporting_asof_date desc
												 limit 1) lcu on true

      left join lateral (select
				                      rdc.data_id,
															rdc.data_value,
															rdc.data_unit,
															rdc.reporting_asof_date
													  from p_rsf.rsf_data_current rdc 
														where rdc.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
				                      and rdc.indicator_id = ind.indicator_id
															and rdc.reporting_asof_date <= input_exchange_rate_date::date
													  order by 
														  rdc.reporting_asof_date desc
															limit 1) as rdc on true

			where fam.child_rsf_pfcbl_id in (0,input_rsf_pfcbl_id) -- As global is only in its own family
			  -- check enforces currency_ratio_data_type_LCU_must_be_denominator LCU in denominator if it's an LCU indicator
				and (ind.data_unit = v_currency_ratio OR
				     coalesce(rdc.data_unit=v_currency_ratio,false) OR
			       p_rsf.fx_currency_ratio_in_alphabetic_order(regexp_replace(ind.data_unit,'LCU',lcu.data_unit_value)) = v_currency_ratio)
						 
			  -- 2024-11-15: programs not subscribed to USD_LCU fx indicator were querying it.
			  -- exists are much faster than joining the subscriptions view
			  and ((ind.data_category = 'global' and fam.parent_rsf_pfcbl_id = 0)
				     or 
             (
				        -- have I ever reported on it (not just subscribed to it!)
							 -- if it's an empty/mistaken subscription then don't return it and then tell us it's missing a data_id
							 -- which will given the system calculation an fx error.
						   exists(select * from p_rsf.rsf_data_current rdc
								           where rdc.indicator_id = ind.indicator_id
													   and rdc.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id)
							 and 
						   -- is the requested entity subscribed to the indicator?
						   -- fx indicators only at facility or global levels
							 -- if facility is UN-subscribed, will return false and return global-level
							 -- if facility is empty, will return subscription at program level (true or false)
							 -- if program is empty, will return NULL is true (false)
						   (select pfi.is_subscribed
							  from p_rsf.rsf_pfcbl_ids ids 
								inner join p_rsf.rsf_program_facility_indicators pfi on pfi.rsf_program_id = ids.rsf_program_id
								where ids.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
								  and pfi.indicator_id = ind.indicator_id
                  and (pfi.rsf_facility_id is NULL or pfi.rsf_facility_id is not distinct from fam.parent_rsf_pfcbl_id)
								order by pfi.rsf_facility_id is NOT NULL DESC
								limit 1) is true
					   )
			   ) 
												 
			order by 
				
				rdc.data_id is not null desc, -- if uses a facility-level indicator but no data submitted, then default to global.
				fam.parent_pfcbl_rank desc,   -- from self closest to global last
				rdc.reporting_asof_date desc, -- if facility uploaded multiple (eg, LCU/USD where LCU=EUR and EUR/USD) then most recent reporting
				ind.data_unit ~ 'LCU' desc    -- facility *could* define LCU/USD and USD/XOF fx rate columns and its LCU is XOF, meaning
				                              -- two facility-level indicators are (presumably) capturing the same fx rate at the same time.
																	    -- hopefully client is reporting these identically...but prefer the defined USD/XOF over the implied LCU/USD
			limit 1;	
			*/
		end if;
		
		is_invalidated := exists(select * 
		                         from p_rsf.rsf_data_calculation_evaluations rdce
														 where rdce.rsf_pfcbl_id = v_fx_rsf_pfcbl_id
														   and rdce.indicator_id = fx_indicator_id
															 and rdce.calculation_asof_date = exchange_rate_date);
															 
		is_unreported := exchange_rate_data_id is NULL 
		                 or
										 not exists(select * 
		                            from p_rsf.rsf_pfcbl_reporting rpr
													      where rpr.rsf_pfcbl_id = v_fx_rsf_pfcbl_id
														      and rpr.reporting_asof_date = exchange_rate_date);
		                        
	end if;
	
	-- if equal, then what we want "currency_ratio" is how it is natively saved.  
	-- but if it's not, then we need to invert the value.
  if (currency_ratio <> v_fx_data_unit_currency_ratio)
	then
	  --raise notice 'converting because % <> %',currency_ratio,v_fx_data_unit_currency_ratio;
		exchange_rate := 1/exchange_rate;
	end if;
	
	--raise notice 'requested=% queried=% returned=% with value=% asof=%',currency_ratio,v_currency_ratio,v_fx_data_unit_currency_ratio,exchange_rate,exchange_rate_date;
	
	return;
	
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for get_data_by_family_tree
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."get_data_by_family_tree"("input_rsf_pfcbl_ids_familytree" _int4, "input_indicator_ids" _int4, "input_current_date" date, "input_to_currency" text, "fx_force_global" bool, "fx_reported_date" bool, "include_flags" bool);
CREATE FUNCTION "p_rsf"."get_data_by_family_tree"("input_rsf_pfcbl_ids_familytree" _int4, "input_indicator_ids" _int4, "input_current_date" date, "input_to_currency" text=NULL::text, "fx_force_global" bool=true, "fx_reported_date" bool=false, "include_flags" bool=true)
  RETURNS TABLE("rsf_pfcbl_id" int4, "pfcbl_category" text, "pfcbl_rank" int2, "parentest_rsf_pfcbl_id" int4, "parentest_pfcbl_category" text, "indicator_id" int4, "data_type" text, "indicator_name" text, "data_id" int4, "data_value" text, "data_unit" text, "data_asof_date" date, "data_value_updated" bool, "exchange_rate_date" date, "fx_indicator_id" int4, "fx_calculation_category" text, "currency_ratio" text, "exchange_rate_data_id" int4, "exchange_rate" numeric, "current_asof_date" date, "current_value" text, "current_unit" text, "flag_ids_active" _int4, "flag_ids_resolved" _int4) AS $BODY$
--declare v_data_categories text[];
BEGIN

input_to_currency := upper(trim(input_to_currency));

input_rsf_pfcbl_ids_familytree := uniq(sort(input_rsf_pfcbl_ids_familytree));

input_indicator_ids := uniq(input_indicator_ids);
/*
select array_agg(x.data_category order by x.pfcbl_rank)
into v_data_categories
from (
  select distinct ind.data_category,ind.pfcbl_rank
  from p_rsf.indicators ind
  where ind.indicator_id = any(array[input_indicator_ids]::int[])
) x;
*/
/*
select array_agg(rpc.pfcbl_category order by rpc.pfcbl_rank)
into v_data_categories
from p_rsf.rsf_pfcbl_categories rpc
where exists(select * from p_rsf.indicators ind
             where rpc.pfcbl_category = ind.data_category
							 and ind.indicator_id = any(array[input_indicator_ids]::int[]));
*/

return query 
  with cats as (
    select 
      x.data_category as pfcbl_category,
      x.pfcbl_rank,
      lag(x.pfcbl_rank, 1) over(order by x.pfcbl_rank) as parentest_pfcbl_rank
    from (
    select distinct ind.data_category,ind.pfcbl_rank
    from p_rsf.indicators ind
    where ind.indicator_id = any(input_indicator_ids)
    order by ind.pfcbl_rank
    ) x
  )
	select
	ids.rsf_pfcbl_id,
	ids.pfcbl_category::text,
	ids.pfcbl_rank::smallint,
	ids.parentest_rsf_pfcbl_id,
	ids.parentest_pfcbl_category,
	ids.indicator_id,
	ids.data_type::text,
	ids.indicator_name::text,
	dc.data_id,
	case when ids.is_periodic_or_flow_reporting is true
	      and ids.data_type in ('number','percent','currency')
				and dc.data_asof_date <> input_current_date
				then '0'::text
			 else dc.data_value 
	end as data_value,
	coalesce(dc.data_unit,ids.data_unit) as data_unit,
	dc.data_asof_date,
	dc.data_asof_date = input_current_date as data_value_updated,
	fx.exchange_rate_date,
	fx.fx_indicator_id,
	fx.fx_pfcbl_category as fx_calculation_category,	
	case when ids.data_type = 'currency' 
	      and input_to_currency IS NOT NULL
				and fx.exchange_rate_data_id IS NULL
				and dc.data_id is not null
			 then 'ERROR'
			 else fx.currency_ratio
  end::text as currency_ratio,
	
	fx.exchange_rate_data_id,
	fx.exchange_rate,
	input_current_date as current_asof_date,
	case when ids.is_periodic_or_flow_reporting is true
	      and ids.data_type in ('number','percent','currency')
				and dc.data_asof_date <> input_current_date
				then '0'::text
			 when fx.exchange_rate_data_id is NOT NULL
			 then ((dc.data_value::numeric) * (fx.exchange_rate))::text -- FX rates outputs FROM/TO will multiply to go FROM -> TO
			 else dc.data_value
	end as current_value,

	case when fx.exchange_rate_data_id is NOT NULL
			 then input_to_currency
			 when fx.exchange_rate_data_id IS NULL  -- can't fx for some reason, but NULL USD and NULL EUR, etc are identical
			      AND 
						dc.data_value is NULL
			then input_to_currency
			 else coalesce(dc.data_unit,ids.data_unit)
	end as current_unit,
	fl.flag_ids_active,
	fl.flag_ids_resolved

	from (
	
  select
		ids.rsf_pfcbl_id,
		ind.indicator_id,
		ind.data_type,
		ind.data_unit,
		ind.indicator_name,
		ind.is_periodic_or_flow_reporting,
		ids.pfcbl_category as pfcbl_category,
		ids.pfcbl_category_rank as pfcbl_rank,
    (array['global','program','facility','client','borrower'])[cats.parentest_pfcbl_rank+1] as parentest_pfcbl_category,
    (array[0,ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id])[cats.parentest_pfcbl_rank+1]
     as parentest_rsf_pfcbl_id
	from p_rsf.rsf_pfcbl_ids ids
  inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
  inner join cats on cats.pfcbl_category = ids.pfcbl_category
  where ids.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id 
                               from p_rsf.view_rsf_pfcbl_id_family_tree ft
                               where ft.from_rsf_pfcbl_id = any(input_rsf_pfcbl_ids_familytree))
    and ind.indicator_id = any(array[input_indicator_ids]::int[])
		and ids.created_in_reporting_asof_date <= input_current_date::date
			
  
  /*
	  select 
		ids.rsf_pfcbl_id,
		ind.indicator_id,
		ind.data_type,
		ind.data_unit,
		ind.indicator_name,
		ind.is_periodic_or_flow_reporting,
		ids.pfcbl_category,
		ids.pfcbl_category_rank as pfcbl_rank,
		NULL::text as parent_pfcbl_category
		
		from p_rsf.rsf_pfcbl_ids ids
		inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
		where ids.pfcbl_category = 'global'
			and ind.indicator_id = any(array[input_indicator_ids]::int[])
			and ids.created_in_reporting_asof_date <= input_current_date::date
			
		union --all
		
		select
		parents.parent_rsf_pfcbl_id as rsf_pfcbl_id,
		ind.indicator_id,
		ind.data_type,
		ind.data_unit,
		ind.indicator_name,
		ind.is_periodic_or_flow_reporting,
		parents.parent_pfcbl_category as pfcbl_category,
		parents.parent_pfcbl_rank as pfcbl_rank,
		(v_data_categories)[(array_position(v_data_categories::text[],parents.parent_pfcbl_category::text)-1)] as parent_pfcbl_category
		from p_rsf.rsf_pfcbl_id_family parents 
		inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = parents.parent_rsf_pfcbl_id
		inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
		where parents.child_rsf_pfcbl_id = any(array[input_rsf_pfcbl_ids_familytree]::int[])
			and parents.parent_pfcbl_rank <= parents.child_pfcbl_rank
			and ind.indicator_id = any(array[input_indicator_ids]::int[])
			and ids.created_in_reporting_asof_date <= input_current_date::date
			
		union --all	

		select
		children.child_rsf_pfcbl_id as rsf_pfcbl_id,
		ind.indicator_id,
		ind.data_type,
		ind.data_unit,
		ind.indicator_name,
		ind.is_periodic_or_flow_reporting,
		children.child_pfcbl_category as pfcbl_category,
		children.child_pfcbl_rank as pfcbl_rank,
		(v_data_categories)[(array_position(v_data_categories::text[],children.child_pfcbl_category::text)-1)] as parent_pfcbl_category
		
		
		from p_rsf.rsf_pfcbl_id_family children 
		inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = children.child_rsf_pfcbl_id
		inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
		where children.parent_rsf_pfcbl_id = any(array[input_rsf_pfcbl_ids_familytree]::int[])
			and children.child_pfcbl_rank > children.parent_pfcbl_rank
			and ind.indicator_id = any(array[input_indicator_ids]::int[])
			and ids.created_in_reporting_asof_date <= input_current_date::date
*/      
	) as ids

	left join lateral (select
											 rdc.data_id,
											 rdc.data_value,
											 rdc.data_unit,
											 rdc.reporting_asof_date as data_asof_date
										 from p_rsf.rsf_data_current rdc
										 where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
											 and rdc.indicator_id = ids.indicator_id
											 and rdc.reporting_asof_date <= input_current_date::date
										 order by rdc.reporting_asof_date desc
										 limit 1) as dc on true
	left join lateral p_rsf.fx_pfcbl_convert_currency(input_rsf_pfcbl_id => ids.rsf_pfcbl_id, 
																						input_from_currency => dc.data_unit,
																						input_to_currency => input_to_currency,
																						input_exchange_rate_date => case when fx_reported_date = true
																						                                 then dc.data_asof_date
																																						 else input_current_date::date end,
																						input_force_global_fx_rate => fx_force_global)	as fx on ids.data_type = 'currency'
																																																 and input_to_currency IS NOT NULL
  left join lateral (select 
											 array_agg(chk.evaluation_id) 
											 filter(where chk.check_status = 'active') as flag_ids_active,
											 
										   array_agg(chk.evaluation_id) 
											 filter(where chk.check_status <> 'active') as flag_ids_resolved
										 	
	                   from p_rsf.rsf_data_checks chk										 
										 where chk.data_id = dc.data_id
										   and chk.check_asof_date = input_current_date::date
											 and include_flags is true
									  ) as fl on include_flags is true;
										
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100
  ROWS 1000;

-- ----------------------------
-- Function structure for get_rsf_data_current
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."get_rsf_data_current"("input_rsf_pfcbl_id" int4, "input_indicator_id" int4, "input_current_date" date, OUT "data_id" int4, OUT "reporting_asof_date" date, OUT "data_value" text, OUT "data_unit" text, OUT "data_unit_data_id" int4);
CREATE FUNCTION "p_rsf"."get_rsf_data_current"(IN "input_rsf_pfcbl_id" int4, IN "input_indicator_id" int4, IN "input_current_date" date=NULL::date, OUT "data_id" int4, OUT "reporting_asof_date" date, OUT "data_value" text, OUT "data_unit" text, OUT "data_unit_data_id" int4)
  RETURNS "pg_catalog"."record" AS $BODY$
begin 
	select
		rdc.data_id,
		rdc.reporting_asof_date,
		rdc.data_value,
		rdc.data_unit,
		rdc.data_unit_data_id
	into data_id,reporting_asof_date,data_value,data_unit,data_unit_data_id
	from p_rsf.rsf_data_current rdc
	where rdc.rsf_pfcbl_id = input_rsf_pfcbl_id
	  and rdc.indicator_id = input_indicator_id
		and coalesce(rdc.reporting_asof_date <= input_current_date,true) -- if NULL then true, ie, moot.
	order by
		rdc.reporting_asof_date desc
	limit 1;
	
	return;
	
end $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for get_rsf_pfcbl_id_by_sys_name
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."get_rsf_pfcbl_id_by_sys_name"("v_sys_name" text);
CREATE FUNCTION "p_rsf"."get_rsf_pfcbl_id_by_sys_name"("v_sys_name" text)
  RETURNS "pg_catalog"."int4" AS $BODY$ 
declare v_pfcbl_name text;
declare v_parent_pfcbl_id int default null;
declare v_id int;
begin

  v_pfcbl_name := trim(regexp_substr(v_sys_name,'[^>]+$'));
  
  select 
  nai.rsf_pfcbl_id
  into v_id
  from p_rsf.rsf_data_current_names_and_ids nai
  where nai.sys_name = v_sys_name 
     or nai.pfcbl_name = v_sys_name
     or nai.pfcbl_name = v_pfcbl_name
  group by nai.rsf_pfcbl_id
  having count(distinct nai.rsf_pfcbl_id)=1;
  /*
  select 
  nai.rsf_pfcbl_id
  into v_id
  from p_rsf.rsf_data_current_names_and_ids nai
  where nai.sys_name = v_sys_name
  limit 1;
  */
  if v_id is not null
  then
    return v_id;
  end if;  
  
  
  

  if v_id is not null
  then
    raise info 'get_rsf_pfcbl_id_by_sys_name located from view_rsf_pfcbl_id_timeline_sys_names for: %',v_sys_name;
    
    return v_id;
  end if;
  
  if (v_sys_name ~ '>') 
  then
    v_parent_pfcbl_id :=  p_rsf.get_rsf_pfcbl_id_by_sys_name(trim((regexp_match(v_sys_name,'^(.*)>.*$'))[1]));
    if (v_parent_pfcbl_id is NULL)
    then
      raise info 'get_rsf_pfcbl_id_by_sys_name failed to locate parent rsf_pfcbl_id for: %',v_sys_name;
      return NULL;
    end if;
  end if;
  
  
  select rsf_pfcbl_id 
  into v_id
  from (
    select distinct 
      nai.rsf_pfcbl_id,
      count(*) over() as matches
    from (select 
            (regexp_match(v_pfcbl_name,'^([a-z]+):'))[1] as pfcbl_category,
            (regexp_match(v_pfcbl_name,'\((.*)\)$'))[1] as reported_id
         ) as lookup
    inner join p_rsf.rsf_data_current_names_and_ids nai on nai."id" = lookup.reported_id
                                                       and nai.pfcbl_category = lookup.pfcbl_category
    where v_parent_pfcbl_id is NULL 
          OR
          nai.rsf_pfcbl_id in (select ft.to_family_rsf_pfcbl_id
                               from p_rsf.view_rsf_pfcbl_id_family_tree ft 
                               where ft.from_rsf_pfcbl_id = v_parent_pfcbl_id
                                 and ft.pfcbl_hierarchy <> 'parent')
  ) ids
  where ids.matches = 1;
  
  if v_id is not null
  then
    raise info 'get_rsf_pfcbl_id_by_sys_name located from reported_id for: %',v_sys_name;
    return v_id;
  end if;
  
  
  select rsf_pfcbl_id 
  into v_id
  from (
    select distinct 
      nai.rsf_pfcbl_id,
      count(*) over() as matches
    from (select 
            (regexp_match(v_pfcbl_name,'^([a-z]+):'))[1] as pfcbl_category,
            (regexp_match(v_pfcbl_name,'^.*:([^(]+).*$'))[1] as reported_name 
         ) as lookup
    inner join p_rsf.rsf_data_current_names_and_ids nai on nai."name" = trim(lookup.reported_name)
                                                       and nai.pfcbl_category = lookup.pfcbl_category
    where v_parent_pfcbl_id is NULL 
          OR
          nai.rsf_pfcbl_id in (select ft.to_family_rsf_pfcbl_id
                               from p_rsf.view_rsf_pfcbl_id_family_tree ft 
                               where ft.from_rsf_pfcbl_id = v_parent_pfcbl_id
                                 and ft.pfcbl_hierarchy <> 'parent')
  ) ids
  where ids.matches = 1;
  
  if v_id is not null
  then
    raise info 'get_rsf_pfcbl_id_by_sys_name located from reported_name for: %',v_sys_name;
  else 
    raise info 'get_rsf_pfcbl_id_by_sys_name NOT FOUND for: %',v_sys_name;
  end if;
  
  return v_id;

end $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for get_rsf_pfcbl_id_reporting_status_asof_date
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."get_rsf_pfcbl_id_reporting_status_asof_date"("input_rsf_pfcbl_id" int4, "input_pfcbl_category" text, "input_current_date" date, OUT "quarter_end_reporting_status" text, OUT "quarter_reporting_expected" bool, OUT "quarter_reporting_exists" bool, OUT "reporting_init_date" date, OUT "reporting_required_start_date" date, OUT "reporting_expected_end_date" date, OUT "entity_completion_date" date);
CREATE FUNCTION "p_rsf"."get_rsf_pfcbl_id_reporting_status_asof_date"(IN "input_rsf_pfcbl_id" int4, IN "input_pfcbl_category" text, IN "input_current_date" date, OUT "quarter_end_reporting_status" text, OUT "quarter_reporting_expected" bool, OUT "quarter_reporting_exists" bool, OUT "reporting_init_date" date, OUT "reporting_required_start_date" date, OUT "reporting_expected_end_date" date, OUT "entity_completion_date" date)
  RETURNS "pg_catalog"."record" AS $BODY$
declare entity_completion_extension_days int default 0;
declare entity_completion_extension_from_date date default null;
declare entity_deactivated_date date default NULL;
declare quarter_end_active_status bool default NULL;
declare quarter_begin_active_status bool default NULL;
declare quarter_is_ending bool default NULL;
declare quarter_is_beginning bool default NULL;
declare recursive_rsf_pfcbl_ids int[] default NULL;
begin 

	input_current_date := (date_trunc('quarter',input_current_date) + interval '3 mons' - interval '1 day')::date;
	
	if input_pfcbl_category in ('global','program','client','borrower')
	then
	
	  -- global determined by facility dates
		if input_pfcbl_category = 'global'
		then
			select array_agg(ids.rsf_pfcbl_id)
			into recursive_rsf_pfcbl_ids
			from p_rsf.rsf_pfcbl_ids ids
			where ids.pfcbl_category = 'facility';
			
			input_pfcbl_category := 'facility';
		
		-- program determined by its facilities 
		elseif input_pfcbl_category = 'program'
		then 
			select array_agg(distinct ids.rsf_facility_id)
			into recursive_rsf_pfcbl_ids
			from p_rsf.rsf_pfcbl_ids ids
			where ids.rsf_program_id = input_rsf_pfcbl_id;
				
			input_pfcbl_category := 'facility';
		-- facility has its own logic
		-- client a mix of facility and its own
		
		-- borrowers determined by its loans dates
		elseif input_pfcbl_category = 'borrower'
		then 

			select array_agg(distinct ids.rsf_loan_id)
			into recursive_rsf_pfcbl_ids
			from p_rsf.rsf_pfcbl_ids ids
			where ids.rsf_borrower_id = input_rsf_pfcbl_id;
				
			input_pfcbl_category := 'loan';
		
		end if;
		-- loans determined by its own logic
		
		select		
			(array_agg(status.quarter_end_reporting_status 
								order by status.quarter_end_reporting_status='ACTIVE' desc,
												 status.quarter_end_reporting_status='INACTIVE' desc,
												 status.quarter_end_reporting_status='PREACTIVE' desc)::text[])[1],
			bool_or(status.quarter_reporting_expected),			
			min(status.reporting_init_date),
			min(status.reporting_required_start_date),
			max(status.reporting_expected_end_date),
			max(status.entity_completion_date)
		into
			quarter_end_reporting_status,
			quarter_reporting_expected,
			reporting_init_date,
			reporting_required_start_date,
			reporting_expected_end_date,
			entity_completion_date
		from (select unnest(recursive_rsf_pfcbl_ids) as rsf_pfcbl_id) as ids
		inner join lateral p_rsf.get_rsf_pfcbl_id_reporting_status_asof_date(input_rsf_pfcbl_id => ids.rsf_pfcbl_id,
                                                                         input_pfcbl_category => input_pfcbl_category,
																																				 input_current_date => input_current_date) as status on true;
    select ids.created_in_reporting_asof_date
		into reporting_init_date
		from p_rsf.rsf_pfcbl_ids ids
		where ids.rsf_pfcbl_id = input_rsf_pfcbl_id;
		
		select exists(select * from p_rsf.rsf_pfcbl_reporting rpr
		              where rpr.rsf_pfcbl_id = input_rsf_pfcbl_id
									  and rpr.reporting_asof_date = input_current_date)
		into quarter_reporting_exists;
		
    if quarter_end_reporting_status IS NULL -- parent entity has no children created to determine status, so use defaults and end "tomorrow"
		then
			if input_current_date < reporting_init_date
			then
				quarter_end_reporting_status := 'PREACTIVE';
				quarter_reporting_expected   := false;
				reporting_required_start_date:= NULL::date;
				reporting_expected_end_date  := reporting_init_date + interval '1 day';
				entity_completion_date := reporting_init_date + interval '1 day';
			else
				quarter_end_reporting_status := 'ACTIVE';
				quarter_reporting_expected   := false;
				reporting_required_start_date:= NULL::date;
				reporting_expected_end_date  := input_current_date + interval '1 day';
				entity_completion_date := input_current_date + interval '1 day';			
			end if;
		end if;
		
    return;																																				 
	end if;

  with status_data as MATERIALIZED (
		select distinct on (rdc.rsf_pfcbl_id,ind.indicator_sys_category)
			rdc.rsf_pfcbl_id,
			rdc.reporting_asof_date,
			ind.indicator_sys_category,
			rdc.data_value,
			rdc.data_unit
		from p_rsf.rsf_data_current rdc
		inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
		where rdc.rsf_pfcbl_id = input_rsf_pfcbl_id
			and rdc.reporting_asof_date <= input_current_date
			and ind.indicator_sys_category in ('reporting_required_start_date',
																				 'entity_completion_date',
																				 'is_active',
																				 'reporting_required_start_date',
																				 'entity_completion_extension_time',
																				 'entity_completion_extension_from_date')
		order by 
			rdc.rsf_pfcbl_id,
			ind.indicator_sys_category,
			rdc.reporting_asof_date desc
	)
	select
		max(sd.data_value) filter(where indicator_sys_category = 'entity_completion_date')::date,		
		max(sd.data_value) filter(where indicator_sys_category = 'reporting_required_start_date')::date,
		
		max(sd.data_value) filter(where indicator_sys_category = 'entity_completion_extension_from_date')::date,
		max(sd.data_value) filter(where indicator_sys_category = 'entity_completion_extension_time')::int,
		max(case when (sd.data_value::bool) is NULL then NULL::date
						 when (sd.data_value::bool) = true  then NULL::date
						 else sd.reporting_asof_date 
				end) filter(where indicator_sys_category = 'is_active')::date
	into
		entity_completion_date,                 -- OUT 
		reporting_required_start_date,          -- OUT 
		entity_completion_extension_from_date,  -- DECLARED
    entity_completion_extension_days,       -- DECLARED
		entity_deactivated_date                 -- DECLARED
		
	from status_data sd
	group by sd.rsf_pfcbl_id;
	
	-- Client termination date the same as facility
	-- Otherwise, client has its own reporting start date
	if input_pfcbl_category = 'client'
	then			
			select sd.data_value::date
			into entity_completion_date
			from p_rsf.rsf_pfcbl_ids ids
			inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_facility_id
			inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
			where ids.rsf_client_id = input_rsf_pfcbl_id
			  and rdc.reporting_asof_date <= input_current_date
				and ind.indicator_sys_category = 'entity_completion_date'
      order by
				rdc.reporting_asof_date desc
			limit 1;
	end if;		
	

	select ids.created_in_reporting_asof_date
		into reporting_init_date
		from p_rsf.rsf_pfcbl_ids ids
		where ids.rsf_pfcbl_id = input_rsf_pfcbl_id;
		
	select exists(select * from p_rsf.rsf_pfcbl_reporting rpr
								where rpr.rsf_pfcbl_id = input_rsf_pfcbl_id
									and rpr.reporting_asof_date = input_current_date)
	into quarter_reporting_exists;
		
	if entity_completion_date is NULL 
	then
		entity_completion_date := greatest(reporting_init_date,input_current_date) + interval '1 day';
	end if;
	
	if entity_completion_extension_days is NULL
	then 
		entity_completion_extension_days:= 0;
	end if;
	
	-- if I report a deactivation date, then as of current date, I have reported ACTIVE=FALSE												
	if (entity_deactivated_date is NOT NULL)    
	then
		quarter_end_active_status := false;
	end if;									

--	 select * from p_rsf.indicators where indicator_sys_category = 'entity_completion_date'
		
	-- previously: loan's default "reporting_expected_end_date" was set to facility's entity_completion_date 
	-- as many loans are reported without final maturity date -- so when NULL, assume reports until end of facility
	-- now, assume it ends "tomorrow"
		

	if (entity_completion_extension_from_date is not null)
	then
		-- if first missed payment date is reported, it cannot be missed after the end date.
		-- sometimes client report it because if how systems or time of reporting goes.
		if entity_completion_extension_from_date > entity_completion_date
		then 
			entity_completion_extension_from_date := entity_completion_date;
			
			-- eg, if 5 days is reported and first missed date is after maturity
			-- then add the difference, eg 7 days from maturity to end of quarter.
			entity_completion_extension_days := entity_completion_extension_days +
																					(entity_completion_date - entity_completion_extension_from_date)::int + 1;
		end if;
		
		-- if its amoritizing and they've missed a loan installment before final maturity, then this isn't an arrears end date extension
		if (entity_completion_extension_from_date + 
				make_interval(days => entity_completion_extension_days::int)) <= entity_completion_date
		then
			entity_completion_extension_days := 0;
		end if;
		
	end if;

	-- we reported overdue days...
	if entity_completion_extension_days is not null and entity_completion_extension_days > 0
	then
		if (entity_completion_date <= input_current_date)
				AND 
				(entity_completion_date + 
				 make_interval(days => entity_completion_extension_days::int)) > (input_current_date) 
		then 
			entity_completion_date := input_current_date;
		else 
			entity_completion_date := entity_completion_date + 
																make_interval(days => entity_completion_extension_days::int);
		end	if;
		
		-- If we reported an extension, then the end date isn't the date of the extension, but the day after.
		entity_completion_date := entity_completion_date + interval '1 day';
		
	end if;			
	
	reporting_expected_end_date :=  least(
	                                  (date_trunc('quarter',reporting_expected_end_date) + interval '3 mons' - interval '1 day')::date,
																		(date_trunc('quarter',entity_completion_date) + interval '3 mons' - interval '1 day')::date,
																		entity_deactivated_date,
																		reporting_expected_end_date
																	);
																	
	quarter_end_active_status := (reporting_init_date <= input_current_date and        -- I started in/before current date
															reporting_expected_end_date > input_current_date and  -- I complete after current date
															entity_deactivated_date is NULL);                     -- I dont report that I'm deactive

 /*
 raise notice 'quarter_end_active_status: % 
               reporting_init_date <= input_current_date=%
							 reporting_expected_end_date > input_current_date=%
							 entity_deactivated_date is NULL=% 
							 entity_deactivated_date <= input_current_date=%',
 quarter_end_active_status,
 reporting_init_date <= input_current_date,
 reporting_expected_end_date > input_current_date,
 entity_deactivated_date is NULL,
 entity_deactivated_date <= input_current_date;
 */
 -- then I expect to be active as-of the end of the quarter
 
  -- Do I end begin on/before the quarter start?  Do I end on/after the quarter end?
	-- If so, I must be active as-of quarter start
 quarter_begin_active_status := reporting_init_date <= date_trunc('quarter',input_current_date) and 
                                reporting_expected_end_date >= date_trunc('quarter',input_current_date);
 
 -- is my begin date sometime this quarter?
 -- I'm beginning this quarter
 quarter_is_ending := (reporting_expected_end_date between date_trunc('quarter',input_current_date) and input_current_date)
                      or 
											coalesce(entity_deactivated_date = input_current_date,false);
 
 quarter_is_beginning := reporting_init_date between date_trunc('quarter',input_current_date) and input_current_date;

 quarter_reporting_expected :=
		case
			
			--(1) When I have a valid completion date AND a valid reporting required date
			-- Then did I start reporting requirements on or before the quarter end date?
			-- And I'm active at the beginning of the quarter
			when entity_completion_date is NOT NULL and reporting_required_start_date IS NOT NULL
			then greatest(reporting_init_date,reporting_required_start_date) <= input_current_date -- VALID START (I start reporting before EOQ)
					 and (quarter_is_ending=true OR quarter_end_active_status=true)                    -- I'm closing in/affter quarter end date
					 
			--(2) When I have a valid completion date, but no specific reporting requirement date
			-- Then did I start being active on or before the quarter end date?
			-- And I'm active at the beginning of the quarter		
			when entity_completion_date is NOT NULL and reporting_required_start_date IS NULL 
			then reporting_init_date <= input_current_date                           -- VALID START (I started before the end of the quarter)
					 and (quarter_is_ending = true OR quarter_end_active_status = true ) -- I'm closing in/affter quarter end date
					 
			--(3) When I do not have a completion date (happens when I should have a completion date but have not set one yet!)
			-- But I am not deliberately disactivated
			-- Then I should expect to report until I upload my completion date or I'm disactivated for some reason
			when entity_completion_date is NULL and entity_deactivated_date is NULL
			then input_current_date >= reporting_init_date
			
			--(4) If I'm deactivated then, nothing is expected.  This comes later in the case due to some observations of loans starting deactivated,
			--which shouldn't be allowed.  So this can help to flag those errors.
			when coalesce(entity_deactivated_date <= input_current_date,false) = true then true
					 
			-- (5) If I'm not activated, and I don't report any specific activity flag, then I expect to report while active
			else input_current_date between reporting_init_date and coalesce(reporting_expected_end_date,input_current_date) -- VALID PERIOD
		end;
/*
	raise notice 'input_current_date=%: 
	               quarter_is_ending=% 
								 quarter_end_active_status=% 
								 reporting_expected_end_date=% 
								 quarter_reporting_expected=% 
								 entity_deactivated_date=%',
								 input_current_date,
								 quarter_is_ending,
								 quarter_end_active_status,
								 reporting_expected_end_date,
								 quarter_reporting_expected,
								 entity_deactivated_date;
*/

  --quarter is ending: reporting_expected_end_date = current_quarter_date OR entity was "deactivated" in this quarter
	quarter_end_reporting_status := 
		case
		-- I've been intentionally deactivated (for better or worse, correct or not)
    when entity_deactivated_date is not NULL
		     AND entity_deactivated_date <= input_current_date then 'INACTIVE'
    -- I'm not expected to close at EoQ and I am expected to be active 
		when quarter_is_ending = false and quarter_end_active_status = true then 'ACTIVE'
		--when quarter_is_ending = true and reporting_expected_end_date >= input_current_date then 'ACTIVE'
		when quarter_is_ending = true and reporting_expected_end_date > input_current_date then 'ACTIVE'

		-- Reporting happened, but outside expected completion timeline.  Generally reflects post-completion reporting or
		-- other reporting anachronisms
		--when reporting_expected_end_date < input_current_date then 'RETROACTIVE'

    -- means they set the flag this quarter but prematurely: enables a flag vs returning INACTIVE 

    -- means they reported data in its final reporting period (as they should, at the least to report its inactive)
		when reporting_expected_end_date <= input_current_date then 'INACTIVE'

		-- nothing reported and nothing expected: this shouldn't enter the reporting timeline at all because reporting_stop_dates
		-- will always reflect the latest actual reporting event -- but this is useful for querying timeseries data from the application later
		--when reporting_expected_end_date < date_trunc('quarter',input_current_date) then 'INACTIVE'

		when reporting_init_date > input_current_date then 'PREACTIVE'


		
		-- When no entry was made; and no entry was required to be made -- but an entry might be anticipated before is required to report
		when quarter_reporting_expected = false
		 and input_current_date between reporting_init_date and least(input_current_date,reporting_expected_end_date)
		 and entity_deactivated_date IS NULL then 'PREACTIVE'
	 
	  when quarter_reporting_expected = true then 'ACTIVE'
		
		else 'ERROR'
	end as quarter_end_reporting_status;																

end $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for gin_extract_query_trgm
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gin_extract_query_trgm"(text, internal, int2, internal, internal, internal, internal);
CREATE FUNCTION "p_rsf"."gin_extract_query_trgm"(text, internal, int2, internal, internal, internal, internal)
  RETURNS "pg_catalog"."internal" AS '$libdir/pg_trgm', 'gin_extract_query_trgm'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gin_extract_value_trgm
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gin_extract_value_trgm"(text, internal);
CREATE FUNCTION "p_rsf"."gin_extract_value_trgm"(text, internal)
  RETURNS "pg_catalog"."internal" AS '$libdir/pg_trgm', 'gin_extract_value_trgm'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gin_trgm_consistent
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gin_trgm_consistent"(internal, int2, text, int4, internal, internal, internal, internal);
CREATE FUNCTION "p_rsf"."gin_trgm_consistent"(internal, int2, text, int4, internal, internal, internal, internal)
  RETURNS "pg_catalog"."bool" AS '$libdir/pg_trgm', 'gin_trgm_consistent'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gin_trgm_triconsistent
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gin_trgm_triconsistent"(internal, int2, text, int4, internal, internal, internal);
CREATE FUNCTION "p_rsf"."gin_trgm_triconsistent"(internal, int2, text, int4, internal, internal, internal)
  RETURNS "pg_catalog"."char" AS '$libdir/pg_trgm', 'gin_trgm_triconsistent'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for global_guidance_subscription
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."global_guidance_subscription"();
CREATE FUNCTION "p_rsf"."global_guidance_subscription"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN


  if (NEW.for_pfcbl_category = 'global')
	then


		insert into p_rsf.rsf_program_facility_check_guidance(rsf_pfcbl_id,
																													indicator_check_guidance_id,
																													rsf_program_id,
																													rsf_facility_id,
																													applied_by_user_id,
																													application_time)
		select 
			0 as rsf_pfcbl_id,
			NEW.indicator_check_guidance_id,
			0 as rsf_program_id,
			NULL as rsf_facility_id,
			(select account_id from p_rsf.view_account_info where users_name = 'RSF SYS Admin') as reporting_user_id,
			TIMEOFDAY()::timestamptz
		on conflict do nothing;	
		
	end if;
	
	return NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for global_indicators_auto_subscribed
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."global_indicators_auto_subscribed"();
CREATE FUNCTION "p_rsf"."global_indicators_auto_subscribed"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

		 
  insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
																										indicator_id,
																										formula_id,
																										rsf_program_id,
																										rsf_facility_id,
																										is_subscribed,
																										is_auto_subscribed)
																										
																										
  select 
		0,
		ind.indicator_id,
		indf.formula_id,
		0,
		NULL::int as rsf_facility_id,
		true as is_subscribed,
		true as is_auto_subscribed
	from p_rsf.indicators ind
	left join p_rsf.indicator_formulas indf on indf.indicator_id = ind.indicator_id
	                                       and indf.is_primary_default = true
  where ind.indicator_id = NEW.indicator_id
	  and ind.data_category = 'global' 
  on conflict(rsf_pfcbl_id,indicator_id)
	do update set is_subscribed = EXCLUDED.is_subscribed,
	              formula_id = EXCLUDED.formula_id;

	return NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for global_reporting
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."global_reporting"();
CREATE FUNCTION "p_rsf"."global_reporting"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
  DECLARE reporting_dates RECORD;
BEGIN

    FOR reporting_dates IN
			select 
      grd.valid_reporting_date
      from p_rsf.rsf_pfcbl_generate_reporting_dates(0,now()::date) grd -- NEW.reporting_asof_date,
      where not exists(select * from p_rsf.rsf_pfcbl_reporting rpr
                       where rpr.rsf_pfcbl_id = 0
                         and rpr.reporting_asof_date = grd.valid_reporting_date)
      order by 
      grd.valid_reporting_date                    
    LOOP
    
       raise notice 'global_reporting creating new reporting_cohort entry for: %',reporting_dates.valid_reporting_date;

       with global_import as (
         insert into p_rsf.reporting_imports(import_rsf_pfcbl_id,
                                             import_pfcbl_category,
                                             import_user_id,
                                             import_time,
                                             import_completed,
                                             reporting_asof_date,
                                             template_id,
                                             file_name,
                                             file_data,
                                             import_comments,
                                             pfcbl_name)
         select 
         0 as rsf_pfcbl_id,
         'global' as import_pfcbl_category,
         (select account_id from p_rsf.view_account_info where users_name = 'RSF SYS Calculator' and is_system_account=true) as import_user_id,
         TIMEOFDAY()::timestamptz as import_time,
         true import_completed,
         reporting_dates.valid_reporting_date as reporting_asof_date,
         ri.template_id,
         ri.file_name,
         ''::bytea,
         concat('Global Reporting Triggered by: ',ri.file_name,' import_id=',ri.import_id) as import_comments,
         'GLOBAL'
         from p_rsf.reporting_imports ri
         where ri.import_id = NEW.import_id
           and not exists(select * 
                          from p_rsf.reporting_imports ri
                          where ri.import_rsf_pfcbl_id = 0
                            and ri.reporting_asof_date = reporting_dates.valid_reporting_date)
         returning 
          reporting_imports.import_id,
          reporting_imports.import_rsf_pfcbl_id,
          reporting_imports.reporting_asof_date,
          reporting_imports.import_user_id
       ),
       global_cohort as (
         insert into p_rsf.reporting_cohorts(import_id,
                                             reporting_rsf_pfcbl_id,
                                             reporting_asof_date,                                    
                                             reporting_user_id,
                                             reporting_time,
                                             reporting_type,
                                             is_reported_cohort,
                                             is_calculated_cohort,
                                             data_asof_date)
         select 
           gi.import_id,
           gi.import_rsf_pfcbl_id as reporting_rsf_pfcbl_id,
           gi.reporting_asof_date,
           gi.import_user_id,
           TIMEOFDAY()::timestamptz as reporting_time,
           1 as reporting_type, -- 1=User import
           true as is_reported_cohort,
           false as is_calculated_cohort,
           gi.reporting_asof_date as data_asof_date
         from global_import gi
         returning 
         reporting_cohorts.reporting_cohort_id,
         reporting_cohorts.reporting_rsf_pfcbl_id,
         reporting_cohorts.reporting_asof_date,
         reporting_cohorts.reporting_user_id
       )
       insert into p_rsf.rsf_data(rsf_pfcbl_id,
                                  reporting_asof_date,
                                  reporting_cohort_id,
                                  indicator_id,
                                  data_value,
                                  data_submitted,
                                  data_source_row_id)
       select 
        gd.reporting_rsf_pfcbl_id as rsf_pfcbl_id,
        gd.reporting_asof_date,
        gd.reporting_cohort_id,
        ind.indicator_id,
        NULL::text as data_value,
        gd.reporting_cohort_id::text,
        gd.reporting_cohort_id::text || 'REPORTING_COHORT_ID'
      from global_cohort gd, p_rsf.indicators ind
      where ind.data_category = 'global' and ind.indicator_sys_category = 'entity_reporting';
		END LOOP;

	return NEW;

END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for global_unreporting
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."global_unreporting"();
CREATE FUNCTION "p_rsf"."global_unreporting"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  if (select setting_value::bool 
	    from p_rsf.view_rsf_program_settings 
			where rsf_program_id =0 
			  and setting_name = 'on_delete_global_unreporting') = true
	then 
		delete from p_rsf.reporting_cohorts rc
		where rc.rsf_program_id = 0 
			and rc.reporting_cohort_id <> 0
			and coalesce(rc.reporting_asof_date > (select max(reporting_asof_date) from p_rsf.reporting_cohorts rc where rc.rsf_program_id <> 0),true);
	end if;
		
	return OLD;
	

END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for gtrgm_compress
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_compress"(internal);
CREATE FUNCTION "p_rsf"."gtrgm_compress"(internal)
  RETURNS "pg_catalog"."internal" AS '$libdir/pg_trgm', 'gtrgm_compress'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_consistent
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_consistent"(internal, text, int2, oid, internal);
CREATE FUNCTION "p_rsf"."gtrgm_consistent"(internal, text, int2, oid, internal)
  RETURNS "pg_catalog"."bool" AS '$libdir/pg_trgm', 'gtrgm_consistent'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_decompress
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_decompress"(internal);
CREATE FUNCTION "p_rsf"."gtrgm_decompress"(internal)
  RETURNS "pg_catalog"."internal" AS '$libdir/pg_trgm', 'gtrgm_decompress'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_distance
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_distance"(internal, text, int2, oid, internal);
CREATE FUNCTION "p_rsf"."gtrgm_distance"(internal, text, int2, oid, internal)
  RETURNS "pg_catalog"."float8" AS '$libdir/pg_trgm', 'gtrgm_distance'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_in
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_in"(cstring);
CREATE FUNCTION "p_rsf"."gtrgm_in"(cstring)
  RETURNS "p_rsf"."gtrgm" AS '$libdir/pg_trgm', 'gtrgm_in'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_options
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_options"(internal);
CREATE FUNCTION "p_rsf"."gtrgm_options"(internal)
  RETURNS "pg_catalog"."void" AS '$libdir/pg_trgm', 'gtrgm_options'
  LANGUAGE c IMMUTABLE
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_out
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_out"("p_rsf"."gtrgm");
CREATE FUNCTION "p_rsf"."gtrgm_out"("p_rsf"."gtrgm")
  RETURNS "pg_catalog"."cstring" AS '$libdir/pg_trgm', 'gtrgm_out'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_penalty
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_penalty"(internal, internal, internal);
CREATE FUNCTION "p_rsf"."gtrgm_penalty"(internal, internal, internal)
  RETURNS "pg_catalog"."internal" AS '$libdir/pg_trgm', 'gtrgm_penalty'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_picksplit
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_picksplit"(internal, internal);
CREATE FUNCTION "p_rsf"."gtrgm_picksplit"(internal, internal)
  RETURNS "pg_catalog"."internal" AS '$libdir/pg_trgm', 'gtrgm_picksplit'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_same
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_same"("p_rsf"."gtrgm", "p_rsf"."gtrgm", internal);
CREATE FUNCTION "p_rsf"."gtrgm_same"("p_rsf"."gtrgm", "p_rsf"."gtrgm", internal)
  RETURNS "pg_catalog"."internal" AS '$libdir/pg_trgm', 'gtrgm_same'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for gtrgm_union
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."gtrgm_union"(internal, internal);
CREATE FUNCTION "p_rsf"."gtrgm_union"(internal, internal)
  RETURNS "p_rsf"."gtrgm" AS '$libdir/pg_trgm', 'gtrgm_union'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for guidance_check_counts
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."guidance_check_counts"();
CREATE FUNCTION "p_rsf"."guidance_check_counts"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  -- update will re-trigger cohort check counts based on updated overwrite status assigned to the guidance
--raise info 'cohort_check_counts %',TG_OP;
  update p_rsf.rsf_data_checks rdc
	set status_time = NOW()
	where indicator_check_guidance_id = NEW.indicator_check_guidance_id;
	
RETURN NULL;								 
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for indicator_currency_unit_valid
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."indicator_currency_unit_valid"();
CREATE FUNCTION "p_rsf"."indicator_currency_unit_valid"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  NEW.pfcbl_rank := (select rpc.pfcbl_rank from p_rsf.rsf_pfcbl_categories rpc where rpc.pfcbl_category = NEW.data_category);
  
	if NEW.data_type = 'currency' AND NEW.data_unit is NULL 
	  then new.data_unit := 'LCU';
	end if;
	

  
	return NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for initialize_global_program
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."initialize_global_program"("v_init_date" date);
CREATE FUNCTION "p_rsf"."initialize_global_program"("v_init_date" date='2017-12-31'::date)
  RETURNS "pg_catalog"."void" AS $BODY$
declare init_cohort_id int;
declare all_permissions int;
begin

if (not exists(select * from p_rsf.view_account_info vai
               where vai.users_name = 'RSF SYS Calculator' and is_system_account=true))
then
	raise exception 'Failed to locate account "RSF SYS Calculator" system account in ARL.arlapplications.accounts';

end if;

if (not exists(select * from p_rsf.view_account_info vai
               where vai.users_name = 'RSF SYS Admin' and is_system_account=true))
then
	raise exception 'Failed to locate account "RSF SYS Admin" system account in ARL.arlapplications.accounts';

end if;

insert into p_rsf.rsf_programs(rsf_program_id) values(0) on conflict do nothing;

select bit_or(permission_value) 
into all_permissions
from users.permission_types;

insert into users.permissions(account_id,rsf_pfcbl_id,sys_name,granted,denied,notes)
select account_id,rsf_pfcbl_id,sys_name,granted,denied,notes
from (
select vai.account_id,NULL as rsf_pfcbl_id,'SYSTEM' as sys_name,all_permissions as granted,0 as denied,'Set via p_rsf.initialize_global_program()' as notes
from p_rsf.view_account_info vai
where vai.users_name = 'RSF SYS Calculator' and is_system_account=true

union all 

select vai.account_id,0 as rsf_pfcbl_id,'global:GLOBAL' as sys_name,all_permissions as granted,0 as denied,'Set via p_rsf.initialize_global_program()'
from p_rsf.view_account_info vai
where vai.users_name = 'RSF SYS Calculator' and is_system_account=true

union all 

select vai.account_id,NULL as rsf_pfcbl_id,'SYSTEM' as sys_name,all_permissions as granted,0 as denied,'Set via p_rsf.initialize_global_program()'
from p_rsf.view_account_info vai
where vai.users_name = 'RSF SYS Admin' and is_system_account=true

union all 

select vai.account_id,0 as rsf_pfcbl_id,'global:GLOBAL' as sys_name,all_permissions as granted,0 as denied,'Set via p_rsf.initialize_global_program()'
from p_rsf.view_account_info vai
where vai.users_name = 'RSF SYS Admin' and is_system_account=true
) uinit
on conflict 
do nothing;


-------------------
      with global_import as (
         insert into p_rsf.reporting_imports(import_rsf_pfcbl_id,
                                             import_pfcbl_category,
                                             import_user_id,
                                             import_time,
                                             import_completed,
                                             reporting_asof_date,
                                             template_id,
                                             file_name,
                                             file_data,
                                             import_comments,
                                             pfcbl_name)
         select 
         0 as rsf_pfcbl_id,
         'global' as import_pfcbl_category,
         vai.account_id as import_user_id,
         TIMEOFDAY()::timestamptz as import_time,
         true import_completed,
         v_init_date as reporting_asof_date,
         tmp.template_id, 
         ''::text,
         ''::bytea,
         concat('Global Reporting Triggered by INITIALIZE') as import_comments,
         'GLOBAL'         
         
         from 
         (select account_id from p_rsf.view_account_info where users_name = 'RSF SYS Calculator' and is_system_account=true) vai,
         (select rt.template_id from p_rsf.reporting_templates rt where rt.template_name = 'RSF-ENTITIES-TEMPLATE') tmp
         where not exists(select * from p_rsf.reporting_imports ri
                          where ri.import_rsf_pfcbl_id = 0
                            and ri.template_id = tmp.template_id
                            and ri.reporting_asof_date = v_init_date)
         returning 
         reporting_imports.import_id,
         reporting_imports.import_rsf_pfcbl_id,
         reporting_imports.reporting_asof_date,
         reporting_imports.import_user_id
       ),
       global_cohort as (
         insert into p_rsf.reporting_cohorts(import_id,
                                             reporting_rsf_pfcbl_id,
                                             reporting_asof_date,                                    
                                             reporting_user_id,
                                             reporting_time,
                                             reporting_type,
                                             is_reported_cohort,
                                             is_calculated_cohort,
                                             data_asof_date)
         select 
           gi.import_id,
           gi.import_rsf_pfcbl_id as reporting_rsf_pfcbl_id,
           gi.reporting_asof_date,
           gi.import_user_id,
           TIMEOFDAY()::timestamptz as reporting_time,
           1 as reporting_type, -- 1=User import
           true as is_reported_cohort,
           false as is_calculated_cohort,
           gi.reporting_asof_date as data_asof_date
         from global_import gi
         returning 
         reporting_cohorts.reporting_cohort_id,
         reporting_cohorts.reporting_rsf_pfcbl_id,
         reporting_cohorts.reporting_asof_date,
         reporting_cohorts.reporting_user_id
       ),
       global_entity as (
       
         insert into p_rsf.rsf_pfcbl_ids(rsf_pfcbl_id,
                                         rsf_program_id,
                                         rsf_facility_id,
                                         rsf_client_id,
                                         rsf_borrower_id,
                                         rsf_loan_id,
                                         pfcbl_category,
                                         pfcbl_category_rank,
                                         created_by_reporting_cohort_id,
                                         created_in_reporting_asof_date)
          select 
           0::int as rsf_pfcbl_id,
           0::int as rsf_program_id,
           NULL::int as rsf_facility_id,
           NULL::int as rsf_client_id,
           NULL::int as rsf_borrower_id,
           NULL::int as rsf_loan_id,
           'global' as pfcbl_category,
           0::int as pfcbl_category_rank,
           gc.reporting_cohort_id as created_by_reporting_cohort_id,
           gc.reporting_asof_date as created_in_reporting_asof_date
           from global_cohort gc
           returning
            rsf_pfcbl_id,
            created_in_reporting_asof_date,
            created_by_reporting_cohort_id            
       ),
       global_reporting as (
       
          insert into p_rsf.rsf_data(rsf_pfcbl_id,reporting_asof_date,reporting_cohort_id,indicator_id,data_value,data_submitted)
          select 
            ge.rsf_pfcbl_id,
            ge.created_in_reporting_asof_date as reporting_asof_date,
            ge.created_by_reporting_cohort_id as reporting_cohort_id,
            ind.indicator_id,
            case when ind.indicator_sys_category = 'entity_reporting' then '{INIT}'
                 when ind.indicator_sys_category = 'name' then 'GLOBAL'
                 else 'ERROR' 
            end as data_value,
            case when ind.indicator_sys_category = 'entity_reporting' then '{INIT}'
                 when ind.indicator_sys_category = 'name' then 'GLOBAL'
                 else 'ERROR' 
            end as data_submitted
          from 
          global_entity ge,
          p_rsf.indicators ind
          where ind.indicator_sys_category in ('entity_reporting','name') -- entity_reporting and name _together_ are required for the sys_names trigger to run
            and ind.data_category = 'global'
          on conflict do nothing
          returning 
            reporting_asof_date,
            reporting_cohort_id
       )
       select distinct reporting_cohort_id
       into init_cohort_id
       from global_reporting;
       
       raise info 'init_cohort_id=%',init_cohort_id;
         
------------      
		if (init_cohort_id is not null) then
      insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
                                                       indicator_id,
                                                       calculation_asof_date)
      select 
        0 as rsf_pfcbl_id,
        ind.indicator_id,
        v_init_date
      from p_rsf.indicators ind
      where ind.is_calculated = true
        and ind.is_system_calculated = false
        and ind.data_category = 'global'
      on conflict do nothing;
    end if;
    
  insert into p_rsf.rsf_program_facility_check_guidance(rsf_pfcbl_id,
                                                        indicator_check_guidance_id,
                                                        rsf_program_id,
                                                        rsf_facility_id,
                                                        applied_by_user_id,
                                                        application_time)
		select 
			0 as rsf_pfcbl_id,
			icg.indicator_check_guidance_id,
			0 as rsf_program_id,
			NULL as rsf_facility_id,
			 (select account_id from p_rsf.view_account_info where users_name = 'RSF SYS Admin') as reporting_user_id,
			 TIMEOFDAY()::timestamptz
		from p_rsf.indicator_check_guidance icg 
		where icg.for_pfcbl_category = 'global'
		on conflict do nothing;

end; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for insert_rsf_facility_id
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."insert_rsf_facility_id"();
CREATE FUNCTION "p_rsf"."insert_rsf_facility_id"()
  RETURNS "pg_catalog"."trigger" AS $BODY$

BEGIN

  
	NEW.rsf_facility_id := (select ids.rsf_facility_id 
	                        from p_rsf.rsf_pfcbl_ids ids 
												  where ids.rsf_pfcbl_id = NEW.reporting_rsf_pfcbl_id);
													
	return NEW;
END; 
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for insert_rsf_pfcbl_id_data
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."insert_rsf_pfcbl_id_data"();
CREATE FUNCTION "p_rsf"."insert_rsf_pfcbl_id_data"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

	msg_time := clock_timestamp();
	raise info 'insert_rsf_pfcbl_id_data trigger: % new entities created.  From insert start %',
	(select count(*) from inserted_ids),
	(select clock_timestamp()-now());

                           
  insert into p_rsf.rsf_data(rsf_pfcbl_id,indicator_id,reporting_asof_date,reporting_cohort_id,data_value,data_unit,data_submitted)	
	select 
	  iids.rsf_pfcbl_id,
		ind.indicator_id,
		iids.created_in_reporting_asof_date,
		iids.created_by_reporting_cohort_id,
		iids.rsf_pfcbl_id::text as data_value,
		ind.data_unit,
    '{CREATED SYSID ' || iids.rsf_pfcbl_id::text || '}' as data_submitted
	from inserted_ids iids 
	inner join p_rsf.indicators ind on ind.data_category = iids.pfcbl_category
	where ind.indicator_sys_category = 'SYSID'
  on conflict do nothing;	
	
	return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for insert_rsf_pfcbl_id_evaluations
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."insert_rsf_pfcbl_id_evaluations"();
CREATE FUNCTION "p_rsf"."insert_rsf_pfcbl_id_evaluations"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN


	msg_time := clock_timestamp();
	raise info 'insert_rsf_pfcbl_id_evaluations trigger: % new entities created.  From insert start %',
	(select count(*) from inserted_ids),
	(select now());
	
	insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
	                                                   indicator_id,
																										 calculation_asof_date)
	select
		iids.rsf_pfcbl_id,
		sis.indicator_id,
		iids.created_in_reporting_asof_date as calculation_asof_date
	from inserted_ids iids 
	inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = iids.rsf_pfcbl_id
                                                             and sis.filter_matched_pfcbl_indicators is true -- because we're not joining any specific indicator
	where sis.is_calculated = true 
	  and sis.is_subscribed = true	
    
	on conflict(rsf_pfcbl_id,indicator_id,calculation_asof_date)
	do nothing;

	raise info 'insert_rsf_pfcbl_id_evaluations initialized rsf_data_calculation_evaluations in %',
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();
	
	
	return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for insert_rsf_pfcbl_id_lcu
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."insert_rsf_pfcbl_id_lcu"();
CREATE FUNCTION "p_rsf"."insert_rsf_pfcbl_id_lcu"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

	msg_time := clock_timestamp();
	raise info 'insert_rsf_pfcbl_id_lcu trigger: % new entities created.  From insert start %',
	(select count(*) from inserted_ids),
	(select clock_timestamp()-now());






	-- having initialized the family, now initalize the new entity's LCU unit, which is overwhelmingly inherited (ie, few loans are in defined currency)
	insert into p_rsf.rsf_data_current_lcu(lcu_unit_data_id,
																				 for_rsf_pfcbl_id,
																				 reporting_asof_date,
																				 data_unit_value,
																				 data_id_pfcbl_rank,
																				 is_defined_lcu)
  select distinct on (iids.rsf_pfcbl_id)
      rdc.data_id as lcu_unit_data_id,
      iids.rsf_pfcbl_id as for_rsf_pfcbl_id,
      iids.created_in_reporting_asof_date as reporting_asof_date,
      rdc.data_value as data_unit_value,
      ft.to_pfcbl_rank as data_id_pfcbl_rank,
      false as is_defined_lcu
    from inserted_ids iids 
    inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = iids.rsf_pfcbl_id
    inner join p_rsf.indicators ind on ind.data_category = ft.to_pfcbl_category
                                   and ind.indicator_sys_category = 'entity_local_currency_unit' -- not defined, therefor is_defined_lcu is false
    inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                         and rdc.indicator_id = ind.indicator_id
    where rdc.reporting_asof_date <= iids.created_in_reporting_asof_date
      and ft.pfcbl_hierarchy <> 'child'
    order by
      iids.rsf_pfcbl_id,
      ft.to_pfcbl_rank desc,  -- ie, first facility=2, then program=1
      rdc.reporting_asof_date desc; -- in case the LCU has changed over time, eg, country devalues or reissues (ie, Ghana Cedi vs Shilling)
                                         
/*                                         
  select distinct on (iids.rsf_pfcbl_id)
  
	  rdc.data_id as lcu_unit_data_id,
		iids.rsf_pfcbl_id as for_rsf_pfcbl_id,
		iids.created_in_reporting_asof_date as reporting_asof_date,
		rdc.data_value as data_unit_value,
		fam.parent_pfcbl_rank as data_id_pfcbl_rank,
		false as is_defined_lcu
	from p_rsf.rsf_pfcbl_id_family fam
	inner join inserted_ids iids on iids.rsf_pfcbl_id = fam.child_rsf_pfcbl_id
	inner join p_rsf.indicators ind on ind.data_category = fam.parent_pfcbl_category
																 and ind.indicator_sys_category = 'entity_local_currency_unit' -- not defined, therefor is_defined_lcu is false
  inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
	                                     and rdc.indicator_id = ind.indicator_id
  where rdc.reporting_asof_date <= iids.created_in_reporting_asof_date
  order by
    iids.rsf_pfcbl_id,
		fam.parent_pfcbl_rank desc,  -- ie, first facility=2, then program=1
		rdc.reporting_asof_date desc; -- in case the LCU has changed over time, eg, country devalues or reissues (ie, Ghana Cedi vs Shilling)
	*/
	raise info 'insert_rsf_pfcbl_id_lcu set default entity LCU value (from existing parent) in %',
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();

	return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for normalize_labels
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."normalize_labels"();
CREATE FUNCTION "p_rsf"."normalize_labels"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
declare normalized_primary text;
BEGIN

	normalized_primary := regexp_replace(trim(NEW.primary_label),'[[:space:]]+',' ');

	NEW.secondary_labels := array[NEW.primary_label] || array[normalized_primary] || NEW.secondary_labels;
	NEW.primary_label := normalized_primary;

  create temp table slabels(label text);
	insert into slabels(label)
  select unnest(NEW.secondary_labels);
	
	update slabels set label = regexp_replace(trim(label),'[[:space:]]+',' ');
	
	insert into slabels(label) 
	select public.unaccent(label) 
	from slabels;
	
	select coalesce(array_agg(distinct label order by label),array[]::text[]) into NEW.secondary_labels from slabels;
	
	drop table slabels;

	NEW.secondary_labels := array_remove(NEW.secondary_labels,NEW.primary_label); -- new.primary_label is normalized label;
	
	return NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for remove_old_label_id
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."remove_old_label_id"();
CREATE FUNCTION "p_rsf"."remove_old_label_id"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN
	delete from p_rsf.label_ids where label_id = OLD.label_id;
	RETURN OLD;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for reporting_cohort_deleted
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."reporting_cohort_deleted"();
CREATE FUNCTION "p_rsf"."reporting_cohort_deleted"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

	--If we're deleting the cohort that created a parent-level entity, then all the cohorts that created any child-level
	--entities must also be deleted.
  
  with dependencies as (
		select distinct crc.reporting_cohort_id
		from p_rsf.rsf_pfcbl_ids pids
		inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = pids.rsf_pfcbl_id
                                                     and ft.pfcbl_hierarchy <> 'parent'
		inner join p_rsf.reporting_cohorts crc on crc.reporting_rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
		where pids.created_by_reporting_cohort_id = OLD.reporting_cohort_id
	)
	delete from p_rsf.reporting_cohorts rc
	using dependencies dep
	where rc.reporting_cohort_id = dep.reporting_cohort_id;
	
	return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for reporting_cohort_group_deleted
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."reporting_cohort_group_deleted"();
CREATE FUNCTION "p_rsf"."reporting_cohort_group_deleted"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default now();
BEGIN

  if not exists(select * from deleted_reporting_cohorts)
	then
		return NULL;
	end if;
	
	--raise notice 'p_rsf.reporting_cohort_deleted(%) TG_OP=% and trigger_depth=%',
  --(select array_agg(reporting_cohort_id) from deleted_reporting_cohorts),TG_OP,pg_trigger_depth();					 

  
--	delete from p_rsf.rsf_pfcbl_reporting_template_row_ids tri
--	where tri.reporting_cohort_id = any(select drc.reporting_cohort_id from deleted_reporting_cohorts drc);
	
	-- delete rsf_pfcbl_ids LAST to ensure reporting family trees remain valid to clear-out stale calculations and whatnot.
	delete from p_rsf.rsf_pfcbl_ids ids
	where ids.created_by_reporting_cohort_id = any(select drc.reporting_cohort_id from deleted_reporting_cohorts drc);
	
	raise notice 'rsf_data_deleted rsf_pfcbl_ids created by this cohort %',(clock_timestamp()-msg_time);
	msg_time:= clock_timestamp();
	
	delete from p_rsf.rsf_data rd
	where rd.reporting_cohort_id = any(select drc.reporting_cohort_id from deleted_reporting_cohorts drc);

	raise notice 'rsf_data_deleted rsf_data! %',(clock_timestamp()-msg_time);
	msg_time:= clock_timestamp();

	


	delete from p_rsf.rsf_data_calculation_evaluations dce
	where not exists(select * from p_rsf.rsf_pfcbl_ids ids 
									 where ids.rsf_pfcbl_id = dce.rsf_pfcbl_id);
									 
	delete from p_rsf.rsf_data_check_evaluations dce
	where not exists(select * from p_rsf.rsf_pfcbl_ids ids 
									 where ids.rsf_pfcbl_id = dce.rsf_pfcbl_id);
	--raise exception 'testing reporting cohort deleted failed';
	return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for reporting_cohorts_insert_info
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."reporting_cohorts_insert_info"();
CREATE FUNCTION "p_rsf"."reporting_cohorts_insert_info"()
  RETURNS "pg_catalog"."trigger" AS $BODY$

BEGIN

	if (NEW.is_reported_cohort = true AND NEW.parent_reporting_cohort_id IS NULL)
	then
		insert into p_rsf.reporting_cohort_info(reporting_cohort_id,
                                            upload_filename)
    values(NEW.reporting_cohort_id,'{MISSING}');
	end if;
	
	RETURN NEW;
	
END; 
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for reporting_cohorts_validate_permissions
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."reporting_cohorts_validate_permissions"();
CREATE FUNCTION "p_rsf"."reporting_cohorts_validate_permissions"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE inserted_rsf_pf_id int;
BEGIN
	
	
	perform users.rsf_pfcbl_id_validate_permissions(validate_account_id => NEW.reporting_user_id,
                                                  validate_rsf_pfcbl_id => NEW.reporting_rsf_pfcbl_id,
																									validate_permission_name => 'WRITE');	
  return NEW;																									
	
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for reset_indicator_formula_ids
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."reset_indicator_formula_ids"();
CREATE FUNCTION "p_rsf"."reset_indicator_formula_ids"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

	-- will trigger set_indicator_formula_ids and update/remove indicator that had a dependency
	

/*	
		update p_rsf.indicators ind
		set is_calculation_parameter = exists(select * from p_rsf.indicator_formulas indf
																					where indf.formula_indicator_id_requirements && array[ind.indicator_id])
		where array[ind.indicator_id] && OLD.formula_indicator_id_requirements;	
*/																					
			update p_rsf.indicator_formulas indf
			set modification_time = now()
			where (indf.formula_indicator_ids || indf.formula_indicator_id_requirements) && array[OLD.indicator_id]
				and indf.indicator_id <> OLD.indicator_id; -- with own indicator_id allowed in formula_sort caused recursion, denied here
			return OLD;
	
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_calculation_evaluation_allowed
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_calculation_evaluation_allowed"();
CREATE FUNCTION "p_rsf"."rsf_data_calculation_evaluation_allowed"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN 
 if (select ids.pfcbl_category from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id) 
    is distinct from
    (select ind.data_category from p_rsf.indicators ind where ind.indicator_id = new.indicator_id)
 then
  raise exception 'Calculation category mistmatch for % to calculate %',
  (select sn.sys_name from p_rsf.view_rsf_pfcbl_id_current_sys_names sn where sn.rsf_pfcbl_id = NEW.rsf_pfcbl_id),
  (select ind.indicator_name from p_rsf.indicators ind where ind.indicator_id = NEW.indicator_id);
  return NULL;  
 end if;
 
 --select * from p_rsf.rsf_data_calculation_evaluations;
 --return NEW;
 
 if (not exists(select * from p_rsf.view_rsf_setup_indicator_subscriptions sis
                where sis.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                  and sis.indicator_id = NEW.indicator_id
                  and sis.is_subscribed is true))
 then
  raise exception 'TESTING: Attempt to calculate unsubscribed indicator for: rsf_pfcbl_id=% indicator_id=% asof=%',
  NEW.rsf_pfcbl_id,NEW.indicator_id,NEW.calculation_asof_date;
 
 end if;
 
 /*
 if (NEW.indicator_id = 157648
     and
     (select sn.rsf_pfcbl_id from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
      where sn.sys_name = 'program:SMALL LOAN GUARANTEE PROGRAM (41038) > facility:SLGP RSF SOGESOL (42221)') = NEW.rsf_pfcbl_id)
      
-- if (NEW.indicator_id in (157388,157648) and NEW.calculation_asof_date <= '2018-09-30')
 then
  raise exception 'TESTING: What is causing this to recalculate its history? rsf_pfcbl_id=% indicator_id=% asof=%',
  NEW.rsf_pfcbl_id,NEW.indicator_id,NEW.calculation_asof_date;
 end if;
 */
 return NEW;
 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_calculation_evaluation_validation
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_calculation_evaluation_validation"();
CREATE FUNCTION "p_rsf"."rsf_data_calculation_evaluation_validation"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN 

 insert into p_rsf.rsf_data_calculation_validations(rsf_pfcbl_id,
                                                               indicator_id,
                                                               calculation_asof_date,
                                                               data_id)
 select 
  rdc.rsf_pfcbl_id,
  rdc.indicator_id,
  rdc.reporting_asof_date,
  rdc.data_id
 from p_rsf.rsf_data_current rdc
 where rdc.rsf_pfcbl_id = OLD.rsf_pfcbl_id
   and rdc.indicator_id = OLD.indicator_id
   and rdc.reporting_asof_date <= OLD.calculation_asof_date
 order by rdc.reporting_asof_date desc 
 limit 1
 on conflict do nothing;
                                                                   
 return OLD;
 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_checks_deleted_archive
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_checks_deleted_archive"();
CREATE FUNCTION "p_rsf"."rsf_data_checks_deleted_archive"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

 if (not exists(select * from deleted)) 
 then
	return NULL;
 end if;
 
 
 

--raise notice 'archiving check %',(OLD.data_id);

	insert into p_rsf.rsf_data_checks_archive(archive_id,
	                                          archive_time,
	                                          sys_name,
																					  rsf_pfcbl_id,
																						indicator_id,
																						indicator_check_id,
																						check_formula_id,
																						check_asof_date,
																						check_status,
																						status_time,
																						check_status_user_id,
																						check_status_comment,
																						check_message,
																						consolidated_from_indicator_id,
																						consolidated_from_indicator_check_id,
																						data_sys_flags,
																						data_value_unit)
	select 
	cae.evaluation_id as archive_id,
	now() as archive_time,
	cae.archive_sys_name as sys_name,
	ids.rsf_pfcbl_id,
	cae.indicator_id,
	cae.indicator_check_id,
	cae.check_formula_id,
	cae.check_asof_date,
	cae.check_status,
	cae.status_time,
	cae.check_status_user_id,
	cae.check_status_comment,
	cae.check_message,
	cae.consolidated_from_indicator_id,
	cae.consolidated_from_indicator_check_id,
	cae.data_sys_flags,
	cae.data_value_unit
	from deleted as cae
	inner join p_rsf.view_account_info vai on vai.account_id = cae.check_status_user_id
	left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = cae.indicator_check_guidance_id
  left join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = cae.rsf_pfcbl_id -- in case the entity is being deleted, insert a NULL
	where cae.archive_sys_name is NOT NULL -- 
	  and cae.data_value_unit is NOT NULL  -- Is archivable
		and cae.check_status_comment is distinct from icg.guidance
	  and vai.is_system_account = false     -- Wasn't somehow set by system (such as by applying a guidance)
    and (cae.check_formula_id is NULL  -- system/reporting check
         or
         exists(select * from p_rsf.indicator_check_formulas icf 
                where icf.check_formula_id = cae.check_formula_id))
          
	on conflict do nothing;
	
	
	RETURN NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_checks_flagged_data_cascade
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_checks_flagged_data_cascade"();
CREATE FUNCTION "p_rsf"."rsf_data_checks_flagged_data_cascade"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

--raise notice 'rsf_data_checks_flagged_data evaluation_id=% flags=%',NEW.evaluation_id,NEW.data_sys_flags;

update p_rsf.rsf_data rd
set data_sys_flags = coalesce(rd.data_sys_flags,0) | NEW.data_sys_flags
where rd.data_id = NEW.data_id
  and rd.reporting_asof_date = NEW.check_asof_date;

return NEW;
	 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_checks_set_archivable
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_checks_set_archivable"();
CREATE FUNCTION "p_rsf"."rsf_data_checks_set_archivable"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

-- Trigger will set the archive name for the check
-- Both signalling that it is eligible to be archived.
-- And also saving any (possibly) timeseries permutations in the rsf_pfcbl_id's name if any name or id data values change over time
-- so that the archive will restore in the correct timeseries as historic datasets are (re)uploaded
  NEW.archive_sys_name := (select nai.sys_name
                           from p_rsf.rsf_data_current_names_and_ids nai
													 where nai.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                             and nai.reporting_asof_date <= NEW.check_asof_date
                           order by nai.reporting_asof_date desc
                           limit 1);
	
	NEW.data_value_unit := (select 
													case when rd.data_value is NULL and rd.data_unit is NULL then '{NOTHING}'
															 when rd.data_value is NULL and rd.data_unit is NOT NULL then rd.data_unit
															 when rd.data_value is NOT NULL and rd.data_unit is NULL then rd.data_value
															 when rd.data_value is NOT NULL and rd.data_unit is NOT NULL then rd.data_value || ' ' || rd.data_unit
													end 
													from p_rsf.rsf_data rd
													where rd.data_id = NEW.data_id);
													
--raise notice 'rsf_data_checks_set_archive_name %',NEW.archive_sys_name;	
return NEW;
	 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_checks_validate_permissions
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_checks_validate_permissions"();
CREATE FUNCTION "p_rsf"."rsf_data_checks_validate_permissions"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN
	
	if (exists(select * from p_rsf.rsf_data_checks_archive dca where dca.archive_id = NEW.evaluation_id))
	then
		return NEW;
	else 
	
		perform users.rsf_pfcbl_id_validate_permissions(validate_account_id => NEW.check_status_user_id,
																										validate_rsf_pfcbl_id => NEW.rsf_pfcbl_id,
																										validate_permission_name => 'WRITE');	
		return NEW;																									
	end if;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_current_fx_modified
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_current_fx_modified"();
CREATE FUNCTION "p_rsf"."rsf_data_current_fx_modified"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

	if (exists(select * from p_rsf.rsf_data_current_fx dcf where dcf.fx_data_id = old.data_id))
	then 

		insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
																											 indicator_id,
																											 calculation_asof_date)
		select 
			dcf.rsf_pfcbl_id,
			dcf.indicator_id,
			dcf.reporting_asof_date
		from p_rsf.rsf_data_current_fx dcf
		where dcf.fx_data_id = OLD.data_id
			and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
								 where rpr.rsf_pfcbl_id = dcf.rsf_pfcbl_id
									 and rpr.reporting_asof_date = dcf.reporting_asof_date)
		on conflict do nothing;								 
		
		delete from p_rsf.rsf_data_current_fx dcf
		where dcf.fx_data_id = OLD.data_id;
							 
  end if;
								 
	return NULL;
		
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_current_lcu_modified
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_current_lcu_modified"();
CREATE FUNCTION "p_rsf"."rsf_data_current_lcu_modified"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

	if (not exists(select * from modified_lcu))
	then
		return null;
	end if;
	
raise info 'rsf_data_current_lcu_modified(%)',TG_OP;
msg_time := clock_timestamp();

	create temp table _temp_modified_lcu(for_rsf_pfcbl_id int,
	                                     reporting_asof_date date,
																			 from_data_id int,
																			 from_unit_value text,
																			 to_data_id int,																			 
																			 to_unit_value text)
	on commit drop;
	
	if TG_OP = 'INSERT'
	then 
	
		insert into _temp_modified_lcu(for_rsf_pfcbl_id,
																	 reporting_asof_date,
																	 from_data_id,
																	 from_unit_value,
																	 to_data_id,
																	 to_unit_value)
		select 
			to_lcu.for_rsf_pfcbl_id,
			to_lcu.reporting_asof_date,
			from_lcu.lcu_unit_data_id,
			from_lcu.data_unit_value,
			to_lcu.lcu_unit_data_id,
			to_lcu.data_unit_value
		from modified_lcu to_lcu
		left join lateral (select  -- left join in case there is not previous (shouldn't happen but came up in testing)
													previous.lcu_unit_data_id,
													previous.data_unit_value
												from p_rsf.rsf_data_current_lcu previous
												where previous.for_rsf_pfcbl_id = to_lcu.for_rsf_pfcbl_id
												  and previous.reporting_asof_date < to_lcu.reporting_asof_date
												order by 
													previous.reporting_asof_date desc
												limit 1) from_lcu on true;
    
	elseif TG_OP = 'DELETE'
	then
	  if exists(select * 
		          from (select distinct
							      rpr.rsf_pfcbl_id,
										rpr.reporting_asof_date
										from p_rsf.rsf_pfcbl_reporting rpr
										where rpr.rsf_pfcbl_id = any(select mlcu.for_rsf_pfcbl_id from modified_lcu mlcu)
										  and rpr.rsf_pfcbl_id <> 0
										) as reporting
							 where not exists(select * from p_rsf.rsf_data_current_lcu lcu
															 where lcu.for_rsf_pfcbl_id = reporting.rsf_pfcbl_id
															   and lcu.reporting_asof_date <= reporting.reporting_asof_date))
	  then		
			raise exception 'Delete from rsf_data_current_lcu resulted in entity not having a defined local currency as of its created_in_reporting_asof_date';
		end if;
		
	  -- delete could conceivably have multiple dates
		insert into _temp_modified_lcu(for_rsf_pfcbl_id,
																	 reporting_asof_date,
																	 from_data_id,
																	 from_unit_value,
																	 to_data_id,
																	 to_unit_value)
		select distinct on (from_lcu.for_rsf_pfcbl_id)
			from_lcu.for_rsf_pfcbl_id,
			from_lcu.reporting_asof_date,
			from_lcu.lcu_unit_data_id,
			from_lcu.data_unit_value,
			to_lcu.lcu_unit_data_id,
			to_lcu.data_unit_value			
		from modified_lcu from_lcu
		left join lateral (select 
													previous.lcu_unit_data_id,
													previous.data_unit_value
												from p_rsf.rsf_data_current_lcu previous
												where previous.for_rsf_pfcbl_id = from_lcu.for_rsf_pfcbl_id
													and previous.reporting_asof_date < from_lcu.reporting_asof_date
												order by 
													previous.reporting_asof_date desc
												limit 1) to_lcu on true
		order by
		from_lcu.for_rsf_pfcbl_id,
		from_lcu.reporting_asof_date asc;
												
	elseif TG_OP = 'UPDATE'
	then
		insert into _temp_modified_lcu(for_rsf_pfcbl_id,
																	 reporting_asof_date,
																	 from_data_id,
																	 from_unit_value,
																	 to_data_id,
																	 to_unit_value)
		select distinct on (to_lcu.for_rsf_pfcbl_id)
			to_lcu.for_rsf_pfcbl_id,
			to_lcu.reporting_asof_date,
			from_lcu.lcu_unit_data_id,
			from_lcu.data_unit_value,
			to_lcu.lcu_unit_data_id,
			to_lcu.data_unit_value
		from modified_lcu to_lcu
		inner join removed_lcu from_lcu on from_lcu.for_rsf_pfcbl_id = to_lcu.for_rsf_pfcbl_id
		                               and from_lcu.reporting_asof_date = to_lcu.reporting_asof_date
    order by to_lcu.for_rsf_pfcbl_id,
             to_lcu.reporting_asof_date asc;
	else
		raise exception 'Unmanaged TG_OP=%',TG_OP;
	end if;
	
	raise info 'rsf_data_current_lcu_modified _temp_modified_lcu=% and modified_lcu size=% in %',
	(select count(*) from _temp_modified_lcu),(select count(*) from modified_lcu),(select clock_timestamp()-msg_time);
	
  /*
  	raise info 'TESTING contents of _temp_modified_lcu: %',		
	(select left(json_agg(row_to_json(js))::text,900)
	 from (	 
	 select * from _temp_modified_lcu	 
	 --select * from p_rsf.view_rsf_pfcbl_currency_units_asof_date where for_rsf_pfcbl_id = 108839 
	 ) as js);
   */
  msg_time := clock_timestamp();

	--alter table _temp_modified_lcu add primary key(for_rsf_pfcbl_id,reporting_asof_date);
	alter table _temp_modified_lcu add primary key(for_rsf_pfcbl_id);
	analyze _temp_modified_lcu;
	
	with updates as MATERIALIZED ( 
		select 
			rdc.data_id,
			tml.to_data_id,
			tml.to_unit_value
		from _temp_modified_lcu tml
		inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = tml.for_rsf_pfcbl_id																			 
		inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id		
		where rdc.reporting_asof_date >= tml.reporting_asof_date
			and ind.data_type = 'currency'
			and ind.data_unit = 'LCU'		
			and rdc.data_unit_data_id is distinct from tml.to_data_id
			and (
						 rdc.data_unit_data_id = tml.from_data_id
						 or
						 rdc.data_unit = tml.from_unit_value
						 or
						 rdc.data_unit = tml.to_unit_value
						)
	)
	update p_rsf.rsf_data_current rdc_u
		 set data_unit_data_id = updates.to_data_id,
				 data_unit = updates.to_unit_value
	from  updates
	where updates.data_id = rdc_u.data_id;
	
	
	raise info 'rsf_data_current_lcu_modified currencies modified in %',
	(select clock_timestamp()-msg_time);
	
  msg_time := clock_timestamp();

	-- this is a bit complicated because user may have submitted, eg, USD/LCU, that converted to USD/GHS and
	-- then in alphaberic order -> GHS/USD and inverted value
  -- then the new LCU unit would replace the old -- but is it still in alphabetic order?  Does the value need to re-invert?
	-- Eg, LCU becomes XOF and XOF/USD would need to change alphabetic order and invert the value, too
	-- (although the value would surely need to be recalulated!)
	with updates as MATERIALIZED (
		select 
			rdc.data_id,
			modified.data_unit,
			modified.data_value
		from _temp_modified_lcu tml
		inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = tml.for_rsf_pfcbl_id
		inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
		inner join lateral (select 
													case when rdc.data_unit ~ tml.from_unit_value
															 then regexp_replace(rdc.data_unit,                          
																									 tml.from_unit_value,
																									 tml.to_unit_value)
															 else rdc.data_unit 
													end	as data_unit) as ratio on true
		inner join lateral (select 
													case when p_rsf.fx_currency_ratio_has_alphabetic_order(ratio.data_unit) = false
															 then p_rsf.fx_currency_ratio_in_alphabetic_order(ratio.data_unit)
															 else ratio.data_unit 
													end as data_unit,
													case when p_rsf.fx_currency_ratio_has_alphabetic_order(ratio.data_unit) = false
															 then (1/(rdc.data_value::numeric))::text
															 else rdc.data_value::text
													end as data_value) as modified on true
		where rdc.reporting_asof_date >= tml.reporting_asof_date
			and ind.data_type = 'currency_ratio'
			and ind.data_unit ~ 'LCU'
			and rdc.data_unit_data_id is distinct from tml.to_data_id
			and (
						 rdc.data_unit_data_id = tml.from_data_id
						 or
						 rdc.data_unit ~ tml.from_unit_value
						 or
						 rdc.data_unit ~ tml.to_unit_value
					)
	
	)
	update p_rsf.rsf_data_current rdc_u
		 set data_unit_data_id = updates.data_id,
			   data_unit = updates.data_unit,
				 data_value = updates.data_value
  from updates
	where updates.data_id = rdc_u.data_id;

	raise info 'rsf_data_current_lcu_modified currency_ratios modified in %',
	(select clock_timestamp()-msg_time);
	
	
  msg_time := clock_timestamp();

  -- recalculate all currency calculations or those with a currency parameter
	-- that are affected by the deletion of a lcu data point
  with quasi_currencies as (
    select ind.indicator_id from p_rsf.indicators ind
    where ind.data_type = 'currency'
    or exists(select *
				      from p_rsf.indicator_formula_parameters ifp
					  	where ifp.indicator_id = ind.indicator_id
						   and ifp.parameter_data_type = 'currency')
  )
	insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
	                                                   indicator_id,
																										 calculation_asof_date)
	select 
		rpr.rsf_pfcbl_id,
		sis.indicator_id,
		rpr.reporting_asof_date
	from p_rsf.rsf_pfcbl_reporting rpr
	inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = rpr.rsf_pfcbl_id
                                                             and sis.filter_matched_pfcbl_indicators is true
	--inner join p_rsf.indicators ind on ind.indicator_id = sis.indicator_id
  where exists(select * from _temp_modified_lcu lcu
	             where lcu.for_rsf_pfcbl_id = rpr.rsf_pfcbl_id
							   and rpr.reporting_asof_date >= lcu.reporting_asof_date)
    and sis.indicator_id = any(select qc.indicator_id from quasi_currencies qc)
	  and sis.formula_id is not null
		and sis.is_subscribed is true
    
  on conflict 
	do nothing;
	
		
  raise info 'rsf_data_current_lcu_modified currecy calculation evaluations %',
	(select clock_timestamp()-msg_time);
	
	
--  msg_time := clock_timestamp();

	--do we want to recheck everything?  I don't think we do...if it's recalculated it will be rechecked
	drop table _temp_modified_lcu;
	
	return NULL;
		
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_current_modified_unchanged
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_current_modified_unchanged"();
CREATE FUNCTION "p_rsf"."rsf_data_current_modified_unchanged"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE redundant_data_id int default NULL;
DECLARE is_calculated bool default FALSE;
DECLARE is_periodic bool default FALSE;
DECLARE formula_fx_date text default NULL;
BEGIN
  -- function reviews current versus previous data values: if they're the same, and if it is NOT a LEGITIMATE flow data point
	-- then delete the current value from rsf_data_current
  select
	 previous.data_id
	 into redundant_data_id
	from
	 (select rdc.data_value,rdc.data_unit,rdc.data_id
		from p_rsf.rsf_data_current rdc
		where rdc.rsf_pfcbl_id = NEW.rsf_pfcbl_id
			and rdc.indicator_id = NEW.indicator_id
			and rdc.reporting_asof_date < NEW.reporting_asof_date
		order by 
			rdc.reporting_asof_date desc
		limit 1
	 ) previous 
	where previous.data_value is not distinct from NEW.data_value
	  and previous.data_unit is not distinct from NEW.data_unit;
		
	-- there's a redundancy observed: now lets check if its an allowable type (or not)
	-- although the following is pretty intensive lookups, we should expect redundant inserts to be quite rare into rsf_data
	-- and therefore mostly only perform this block when it is valid.
  if (redundant_data_id IS NOT NULL)
  then
	  
		select 
			coalesce(sis.is_calculated,false),
			coalesce(ind.is_periodic_or_flow_reporting,false) OR coalesce(ind.indicator_sys_category = 'entity_reporting',false),
			coalesce(indf.formula_fx_date,'none') 
			into
			is_calculated,
			is_periodic,
			formula_fx_date
		from p_rsf.view_rsf_setup_indicator_subscriptions sis 
    -- important to return either/both subscribed, unsubscribed -- although should at least be auto-subscribed if we are at this point
    -- due to auto-subscriptions set in previous trigger
		inner join p_rsf.indicators ind on ind.indicator_id = sis.indicator_id
		left join p_rsf.indicator_formulas indf on indf.formula_id = sis.formula_id
		where sis.rsf_pfcbl_id = NEW.rsf_pfcbl_id
		  and sis.indicator_id = NEW.indicator_id;
		
		-- if it's periodic data, allow user data whenever it's reported (data_value_is_meaningfully_different will filter based on ACTIVE status)
		-- but if it's calculated, only allow if onder if its parameters actually triggered the calculation -- including an fx pseudo parameter
		if (is_periodic = true)
		then
			if (is_calculated = false)
			then
				-- reset as we've learned it's not to be considered redundant because a user reported it and it passed meaningfully different
				redundant_data_id := NULL;
			else -- is_calculated = true
			  -- if its a fx-triggered calculation, then it will be triggered by entity reporting.
				-- the main risk here to avoid is that a user submits a user-calculated update in Excel that shouldn't be re-calculated
				-- but they're submitting a change on a periodic data point due to maybe an excel template adjustment and that re-calculation
				-- shouldn't have happened in the first place.
				if (formula_fx_date = 'fx')
				then
					redundant_data_id := NULL;
				else 
					with pids as MATERIALIZED (
					select 
						comp.to_parameter_pfcbl_category,
						comp.to_parameter_rsf_pfcbl_id as parameter_rsf_pfcbl_id
					from p_rsf.compute_calculation_to_parameter_rsf_pfcbl_ids comp
					where comp.from_calculate_rsf_pfcbl_id = NEW.rsf_pfcbl_id
						and comp.from_calculate_indicator_id = NEW.indicator_id
						and comp.parameter_rsf_pfcbl_id_created_date <= NEW.reporting_asof_date
					),
					params as NOT MATERIALIZED (
						select 
							pids.parameter_rsf_pfcbl_id,
							ifp.parameter_indicator_id
						from p_rsf.indicator_formula_parameters ifp
						inner join pids on pids.to_parameter_pfcbl_category = ifp.parameter_pfcbl_category
						where ifp.indicator_id = NEW.indicator_id				
					)
					select exists(select * from p_rsf.rsf_data rd
												where exists(select * from params
																		 where params.parameter_rsf_pfcbl_id = rd.rsf_pfcbl_id
																			 and params.parameter_indicator_id = rd.indicator_id
																			 and rd.reporting_asof_date = NEW.reporting_asof_date))::bool
					into is_periodic; -- recycling variable
				
					if (is_periodic = true)
					then 
						redundant_data_id := NULL;
					end if;
				end if; -- end is fx date
			end if; -- end is_calculated		
		end if;	-- end is_periodic		
	end if; -- end is_redundant
	
	-- Now we're sure its redundant
	if (redundant_data_id is NOT NULL)
	then
	  -- Because the "current" data being inserted is (becoming) equal to the last data that already exists.
		-- Presumably, this happens because a correction is made in the current timeline that says the value didn't change since the last timeline;
		-- effectively, reporting "nothing changed" and therefore, delete the current value that (mistakenly) reports there IS a change
		-- TODO: adjust for flow/periodic reporting
	  delete from p_rsf.rsf_data_current rdc
		where rdc.rsf_pfcbl_id = NEW.rsf_pfcbl_id
		  and rdc.indicator_id = NEW.indicator_id
			and rdc.reporting_asof_date = NEW.reporting_asof_date;
			
		/*raise info 'Redundant insertion omitted and reverted for rsf_pfcbl_id=% indicator_id=% asof=% for % %',
		NEW.rsf_pfcbl_id,
		NEW.indicator_id,
		NEW.reporting_asof_date,
		NEW.data_value,
		NEW.data_unit;
		*/
		return NULL;
	else
		return NEW;
	end if;
	
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_current_names_and_ids_modified
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_current_names_and_ids_modified"();
CREATE FUNCTION "p_rsf"."rsf_data_current_names_and_ids_modified"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE id_indicators int[] default NULL;
BEGIN


  if (not exists(select * from modified)) then
		return NULL;
	end if;
	
  select array_agg(indicator_id)
  into id_indicators
  from p_rsf.indicators ind 
  where ind.indicator_sys_category in ('id','rank_id','name','nickname');
    
    
  if (exists(select * from modified where modified.indicator_id = any(id_indicators))) 
  then
    create temp table _modified_ids as
    select distinct 
    modified.rsf_pfcbl_id,
    modified.reporting_asof_date
    from modified
    where modified.indicator_id = any(id_indicators)
      and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
							   where rpr.rsf_pfcbl_id = modified.rsf_pfcbl_id
						       and rpr.reporting_asof_date = modified.reporting_asof_date);
                   
  with sys_category_indicators as (
    select 
      ind.indicator_id,
      ind.indicator_sys_category,
      ind.data_category    
    from p_rsf.indicators ind    
    where ind.indicator_id = any(id_indicators)
  )
  insert into p_rsf.rsf_data_current_names_and_ids(rsf_pfcbl_id,reporting_asof_date,"id",rank_id,"name",nickname,pfcbl_category,pfcbl_name)	
	select 
	updated.rsf_pfcbl_id,
	updated.reporting_asof_date,
	trim(max(regexp_replace(cd.data_value,'^(.*)#[[:digit:]]+$','\1','g')) filter (where ind.indicator_sys_category = 'id')) as "id",
	max(cd.data_value) filter (where ind.indicator_sys_category = 'rank_id') as rank_id,
	--max(cd.data_value) filter (where ind.indicator_sys_category = 'name') as "name",
  trim((max(regexp_replace(regexp_replace(cd.data_value,'[^A-Za-z0-9[:space:]''&.-]',' ','g'),'[[:space:]]{2,}',' ','g')) filter (where ind.indicator_sys_category = 'name'))) as "name",
	max(cd.data_value) filter (where ind.indicator_sys_category = 'nickname') as "nickname",
  ids.pfcbl_category,
  
  -- if no ID data has been submitted, then the pfcbl_name must be null, else unique index conflicts can arise from concat(ids.pfcbl_category...
  case when max(cd.data_value) filter (where ind.indicator_sys_category = 'rank_id' AND ind.data_category = 'loan') is NULL
        and max(cd.data_value) filter (where ind.indicator_sys_category = 'id') is NULL
        and max(cd.data_value) filter (where ind.indicator_sys_category = 'name') is NULL
        then concat(ids.pfcbl_category || ':SYSID',updated.rsf_pfcbl_id) -- this moots having a sys name since rsf_pfcbl_id is a sequence. On the other hand, this should get promptly overwritten
                                                                         -- and only exist briefly for a newly created entity
       else 
  concat(ids.pfcbl_category || ':',
         coalesce('RANK' || max(cd.data_value) filter (where ind.indicator_sys_category = 'rank_id' AND ind.data_category = 'loan'),
                            trim(max(regexp_replace(regexp_replace(cd.data_value,'[^A-Za-z0-9[:space:]''&.-]',' ','g'),'[[:space:]]{2,}',' ','g')) 
                            filter (where ind.indicator_sys_category = 'name'))),
				 ' (' || trim(max(regexp_replace(cd.data_value,'^(.*)#[[:digit:]]+$','\1','g')) filter (where ind.indicator_sys_category = 'id') || ')')) 
  end as pfcbl_name
  
	from _modified_ids as updated
  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = updated.rsf_pfcbl_id
	inner join sys_category_indicators ind on ind.data_category = ids.pfcbl_category
	left join lateral (select rdc.data_value
	                   from p_rsf.rsf_data_current rdc 
										 where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
	                     and rdc.indicator_id = ind.indicator_id
											 and rdc.reporting_asof_date <= updated.reporting_asof_date
										 order by
										 rdc.reporting_asof_date desc
										 limit 1) as cd on true    
	group by
	updated.rsf_pfcbl_id,
	updated.reporting_asof_date,
  ids.pfcbl_category
	on conflict(rsf_pfcbl_id,reporting_asof_date)
	do update
	set "id" = excluded."id",
	    rank_id = excluded.rank_id,
			"name" = excluded."name",
			nickname = excluded.nickname,
      pfcbl_category = excluded.pfcbl_category, -- shouldn't ever change, but also no fk
      pfcbl_name = excluded.pfcbl_name; 
	
	delete from p_rsf.rsf_data_current_names_and_ids cids
	using _modified_ids
	where _modified_ids.rsf_pfcbl_id = cids.rsf_pfcbl_id
	  and _modified_ids.reporting_asof_date = cids.reporting_asof_date
		and cids."id" is NULL and cids.rank_id is NULL and cids."name" is NULL and cids.nickname is NULL;
		
    
  with sys_names as (
    select 
    ids.rsf_pfcbl_id,
    mids.reporting_asof_date,
    array_to_string(array_agg(p_nai.pfcbl_name order by parent.rsf_pfcbl_id asc),' > ') as sys_name
    from _modified_ids mids
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = mids.rsf_pfcbl_id    
    inner join lateral (select unnest((array[ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id,ids.rsf_loan_id])) as rsf_pfcbl_id) as parent on true
    left join lateral(select nids.pfcbl_name
                      from p_rsf.rsf_data_current_names_and_ids nids 
                      where nids.rsf_pfcbl_id = parent.rsf_pfcbl_id
                        and nids.reporting_asof_date <= mids.reporting_asof_date::date 
                        and parent.rsf_pfcbl_id is not null
                      order by nids.reporting_asof_date desc
                      limit 1) as p_nai on true
    group by ids.rsf_pfcbl_id,mids.reporting_asof_date
  )
  update p_rsf.rsf_data_current_names_and_ids cni
  set sys_name = sn.sys_name
  from sys_names sn
  where sn.rsf_pfcbl_id = cni.rsf_pfcbl_id
    and sn.reporting_asof_date = cni.reporting_asof_Date;
  
  drop table _modified_ids;
  
  end if;
  
	return NULL;
	
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_deleted_data_current
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_deleted_data_current"();
CREATE FUNCTION "p_rsf"."rsf_data_deleted_data_current"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN


 if (not exists(select * from deleted_rsf_data))
	then
		return null;
	end if;
	

	msg_time := clock_timestamp();
	
  create temp table _temp_current_data(data_id int,
	                                     rsf_pfcbl_id int,
																			 indicator_id int,
																			 reporting_asof_date date,
																			 reporting_cohort_id int,
																			 data_time timestamptz,
																			 data_value text,
																			 data_unit text,
																			 data_unit_data_id int,
																			 data_reporting_flags int,
																			 data_type text,
																			 indicator_sys_category text,
																			 data_category text)
  on commit drop;
									
  -- selects still existing rsf_data for the same entity/indicator/date that is not flagged as bit-2 (soft-deleted)
	-- as this pre-existing data will now become the current data and will update rsf_data_current																 
  insert into _temp_current_data(data_id,
	                               rsf_pfcbl_id,
																 indicator_id,
																 reporting_asof_date,
																 reporting_cohort_id,
																 data_time,
																 data_value,
																 data_unit,
																 data_unit_data_id,
																 data_reporting_flags,
																 data_type,
																 indicator_sys_category,
																 data_category)
		select distinct on (rd.rsf_pfcbl_id,rd.indicator_id,rd.reporting_asof_date)
			rd.data_id,
			rd.rsf_pfcbl_id,
			rd.indicator_id,
			rd.reporting_asof_date,
			rd.reporting_cohort_id,
			now()::timestamptz as data_time,
			rd.data_value,
			rd.data_unit,
			NULL::int,
			0::int,
			ind.data_type,
			ind.indicator_sys_category,
			ind.data_category
		from p_rsf.rsf_data rd
		inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
		inner join deleted_rsf_data drd on drd.rsf_pfcbl_id = rd.rsf_pfcbl_id
																	 and drd.indicator_id = rd.indicator_id
																	 and drd.reporting_asof_date = rd.reporting_asof_date
    where (coalesce(rd.data_sys_flags,0) & 2)=0
      and drd.data_id <> rd.data_id -- shouldn't happen anyway, right?																	 
			and exists(select * from p_rsf.rsf_data_current rdc 
			           where rdc.data_id = drd.data_id) -- we want to re-insert only if/where it is in current data.
																									-- if it is not current, let the delete continue without revising current data
																								  -- as the delete has no effect.		
		order by 
			rd.rsf_pfcbl_id,
			rd.indicator_id,
			rd.reporting_asof_date,
			rd.data_id desc;
	
	alter table _temp_current_data add primary key(rsf_pfcbl_id,indicator_id,reporting_asof_date);
	create index _temp_current_data_isc_idx on _temp_current_data(indicator_sys_category);
	
	analyze _temp_current_data;


  -- imagine a user inserts
	-- value=1 at time=1
	-- value=2 at time=2
	-- value=3 at time=2
	-- value=2 at time=1
	-- now deletes
	-- value=3 at time=2
	-- this would find that value=2 is current at both time=1 and time=2, so this is a redundancy and not an inversion to insert
	create temp table _temp_redundancies(rsf_pfcbl_id int,
	                                     indicator_id int,
																			 reporting_asof_date date);
																			 
  insert into _temp_redundancies(rsf_pfcbl_id,
	                               indicator_id,
																 reporting_asof_date)
	select
		tcd.rsf_pfcbl_id,
		tcd.indicator_id,
		tcd.reporting_asof_date	
	from _temp_current_data tcd
	inner join lateral (select
												rdc.data_value is NOT DISTINCT from tcd.data_value
												AND
												rdc.data_unit is NOT DISTINCT from tcd.data_unit
												as reverted													
											from p_rsf.rsf_data_current rdc
											where rdc.rsf_pfcbl_id = tcd.rsf_pfcbl_id
												and rdc.indicator_id = tcd.indicator_id
												and rdc.reporting_asof_date < tcd.reporting_asof_date
											order by rdc.reporting_asof_date desc
											limit 1) as changes on changes.reverted = true;

  -- if the to-be-added value will create a redundancy, then delete it from the inserted/updated data at this timeline
	delete from _temp_current_data tcd
	where exists(select * from _temp_redundancies tr
							 where tr.rsf_pfcbl_id = tcd.rsf_pfcbl_id
							   and tr.indicator_id = tcd.indicator_id
								 and tr.reporting_asof_date = tcd.reporting_asof_date);

  -- and also delete the entry from rsf_data_current since the new entry in rsf_data is a non change.
	delete from p_rsf.rsf_data_current rcd
	where exists(select * from _temp_redundancies tr
							 where tr.rsf_pfcbl_id = rcd.rsf_pfcbl_id
							   and tr.indicator_id = rcd.indicator_id
								 and tr.reporting_asof_date = rcd.reporting_asof_date);

  drop table _temp_redundancies;
									 
  raise info '_temp_current_data reverting to earlier timeline (% data_ids) identified in %',
	(select count(*) from _temp_current_data),
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();

  -- if currency unit definitions are being deleted -- and there isn't a same-asof-date entry to revert to,
	-- then we must also see if the entity's parent entity has an entry to revert to for this as-of date, 
	-- else it will not be captured.
	-- if either local currency unit or defined currency unit is deleted without a reversion, then we only need to worry about
	-- local currency unit at a parent level 
	if exists(select * from deleted_rsf_data drd
	          inner join p_rsf.indicators ind on ind.indicator_id = drd.indicator_id
						where ind.indicator_sys_category in ('entity_local_currency_unit',
						                                     'entity_currency_unit')
					    and not exists(select * from _temp_current_data tcd
							               where tcd.rsf_pfcbl_id = drd.rsf_pfcbl_id
														   and tcd.indicator_id = ind.indicator_id
															 and tcd.reporting_asof_date = drd.reporting_asof_date))
  then
	
     with unreverted_lcu as MATERIALIZED (		
			 select
			   -- indicator_id not relevant here as it would be for the entity being deleted; we need to check reversions to parent level
				 drd.rsf_pfcbl_id,
				 drd.reporting_asof_date
			 from deleted_rsf_data drd
			 inner join p_rsf.indicators ind on ind.indicator_id = drd.indicator_id
			 where ind.indicator_sys_category in ('entity_local_currency_unit',
																						'entity_currency_unit')
				 and not exists(select * from _temp_current_data tcd
												where tcd.rsf_pfcbl_id = drd.rsf_pfcbl_id
													and tcd.indicator_id = ind.indicator_id
													and tcd.reporting_asof_date = drd.reporting_asof_date)
		 )
		 insert into p_rsf.rsf_data_current_lcu(lcu_unit_data_id,
																					  for_rsf_pfcbl_id,
																					  reporting_asof_date,
																					  data_unit_value,
																					  data_id_pfcbl_rank,
																					  is_defined_lcu)
		 select distinct on (ulcu.rsf_pfcbl_id,ulcu.reporting_asof_date)
			 rdc.data_id,
			 ulcu.rsf_pfcbl_id as for_rsf_pfcbl_id,
			 ulcu.reporting_asof_date,
			 rdc.data_value as data_unit_value,
			 ft.to_pfcbl_rank as data_id_pfcbl_rank,
			 false as is_defined_lcu 		 
		 from unreverted_lcu ulcu
     inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = ulcu.rsf_pfcbl_id
                                                      and ft.pfcbl_hierarchy <> 'child'
		 inner join p_rsf.indicators ind on ind.data_category = ft.to_pfcbl_category
		 inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
																					and rdc.indicator_id = ind.indicator_id
																					and rdc.reporting_asof_date = ulcu.reporting_asof_date
		 where ind.indicator_sys_category = 'entity_local_currency_unit'
       
		   and rdc.data_value is not null
		 order by
			ulcu.rsf_pfcbl_id,
			ulcu.reporting_asof_date,
			ft.to_pfcbl_rank desc
                                            
                                            
        /*                                    
		 select distinct on (ulcu.rsf_pfcbl_id,ulcu.reporting_asof_date)
			 rdc.data_id,
			 ulcu.rsf_pfcbl_id as for_rsf_pfcbl_id,
			 ulcu.reporting_asof_date,
			 rdc.data_value as data_unit_value,
			 fam.parent_pfcbl_rank as data_id_pfcbl_rank,
			 false as is_defined_lcu 		 
		 from unreverted_lcu ulcu
		 inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = ulcu.rsf_pfcbl_id
		 inner join p_rsf.indicators ind on ind.data_category = fam.parent_pfcbl_category
		 inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
																					and rdc.indicator_id = ind.indicator_id
																					and rdc.reporting_asof_date = ulcu.reporting_asof_date
		 where ind.indicator_sys_category = 'entity_local_currency_unit'
		   and rdc.data_value is not null
		 order by
			ulcu.rsf_pfcbl_id,
			ulcu.reporting_asof_date,
			fam.parent_pfcbl_rank desc
      */
		on conflict(for_rsf_pfcbl_id,reporting_asof_date) -- if a conflict we know we want to update because defined units always take presidence and
																											-- will always be equal to the entity's own rank
		do update
		set lcu_unit_data_id = EXCLUDED.lcu_unit_data_id,
				data_unit_value = EXCLUDED.data_unit_value,
				data_id_pfcbl_rank = EXCLUDED.data_id_pfcbl_rank,
				is_defined_lcu = EXCLUDED.is_defined_lcu;
	
		 --actually, it's possible that a facility could delete a defined-lcu value and need to revert to its own local value.  
		 --While extremely unlikley, possible
		 --fam.parent_pfcbl_rank < fam.child_pfcbl_rank -- not at own level because it would be in reversions.
		 
	end if;
																									 
	perform p_rsf.function_rsf_data_current_update();

  drop table _temp_current_data;
	
	msg_time := clock_timestamp();
	

--raise info 'deleted_rsf_data contents: %',(select json_agg(tab) FROM (SELECT * from deleted_rsf_data) tab);
--raise info '_temp_delete_ids contents: %',(select json_agg(tab) FROM (SELECT * from _temp_delete_ids) tab);
	
  --alter table _temp_delete_ids add primary key(data_id);
	--analyze _temp_delete_ids;
										 

	delete from p_rsf.rsf_data_current_lcu lcu
	where exists(select * from deleted_rsf_data drd
	             where drd.data_id = lcu.lcu_unit_data_id);
 
 	raise info 'rsf_data_deleted_data_current: deleted rsf_data_current_lcu in %',
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();
	
	create temp table _temp_loan_issuances(rsf_pfcbl_id int,
																				 data_id int,
																				 series_id text);
																	 
  with issuances as MATERIALIZED (
		select 
			drd.rsf_pfcbl_id,
			--rdc.indicator_id,
			cid.data_id,
			--siblings.parent_rsf_pfcbl_id,
			siblings.matched_rsf_pfcbl_id,
			siblings.matched_data_id,
			siblings.series_id
		from deleted_rsf_data drd
		inner join p_rsf.indicators ind on ind.indicator_id = drd.indicator_id
		left join lateral (select 
												rdc.data_id,
												rdc.data_value
											 from p_rsf.rsf_data_current rdc
											 where rdc.rsf_pfcbl_id = drd.rsf_pfcbl_id
											   and rdc.indicator_id = drd.indicator_id
												 and rdc.data_id <> drd.data_id
											 order by rdc.reporting_asof_date desc
											 limit 1) as cid on true
		left join lateral p_rsf.function_rsf_loan_issuance_series(input_rsf_pfcbl_id => drd.rsf_pfcbl_id,
																															input_id_value => cid.data_value,
																															input_id_indicator_id => drd.indicator_id) as siblings on true 
		where ind.indicator_sys_category = 'id' 
			and ind.data_category = 'loan'		
	)
	insert into _temp_loan_issuances(rsf_pfcbl_id,
	                                 data_id,
																	 series_id)
	select 
		lis.rsf_pfcbl_id,																 
		min(lis.data_id) as data_id, -- since cid above left joins on <> drd.data_id, could return an active and NULL data_id
		min(lis.series_id) as series_id
	from (
		select 
		iss.rsf_pfcbl_id,
		iss.data_id,
		iss.series_id
		from issuances iss

		union

		select 
		iss.matched_rsf_pfcbl_id,
		iss.matched_data_id,
		iss.series_id
		from issuances iss
		where series_id is not NULL
		
	) lis
	group by
		lis.rsf_pfcbl_id;

	alter table _temp_loan_issuances add primary key(rsf_pfcbl_id);
	analyze _temp_loan_issuances;
	
	delete from p_rsf.rsf_loan_issuance_series lis
	where exists(select * from _temp_loan_issuances tli
	             where tli.series_id is NULL
							   and tli.rsf_pfcbl_id = lis.rsf_pfcbl_id);

  delete from _temp_loan_issuances tli 
	where tli.series_id is NULL;
	
	insert into p_rsf.rsf_loan_issuance_series(rsf_pfcbl_id,
																							loan_issuance_series_id,
																							loan_issuance_series_rank,
																							id_value_data_id)
  select 
		tli.rsf_pfcbl_id,
		tli.series_id,
		dense_rank() over(partition by tli.series_id order by tli.rsf_pfcbl_id) as series_rank,
		tli.data_id as id_value_data_id
  from _temp_loan_issuances tli		
	on conflict(rsf_pfcbl_id)
	do update
	set loan_issuance_series_id = EXCLUDED.loan_issuance_series_id,
	    loan_issuance_series_rank = EXCLUDED.loan_issuance_series_rank,
			id_value_data_id = EXCLUDED.id_value_data_id;

	delete from p_rsf.rsf_loan_issuance_series lis
	where exists(select * from deleted_rsf_data drd
	             where drd.data_id = lis.id_value_data_id);	

  drop table _temp_loan_issuances;
	
 	raise info 'rsf_data_deleted_data_current: deleted rsf_loan_issuance_series in %',
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();

  -- updated rsf_data_current fk to calculations should update the data_id automatically
	-- however, entries that are being fully deleted should revert to the last-available data_id 
  -- and those reverted ones will not be present in rsf_data_current by this point.										
  delete from p_rsf.rsf_data_current rdc
	where exists(select * from deleted_rsf_data drd
	             where drd.data_id = rdc.data_id);

  																 
 	raise info 'rsf_data_deleted_data_current: deleted rsf_data_current in %',
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();
		
	-- delete indicators this program has auto-subscribed through via reporting uploads
	-- for which those auto-subscribed indicators no longer exist as those reports are deleted.
  /* this functionality is now tied to the reporting_cohort_id foreign key
  with deleted_indicators as (
    select distinct 
    pis.subscription_rsf_pfcbl_id,
    pis.indicator_id
    from deleted_rsf_data drd
    inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = drd.rsf_pfcbl_id
                                                               and pis.indicator_id = drd.indicator_id                                                           
    where pis.is_auto_subscribed is true                                                           
  ),
  deleted_subscriptions as (
    select 
    di.subscription_rsf_pfcbl_id,
    di.indicator_id
    from deleted_indicators di 
    where not exists(select * from p_rsf.rsf_data rd
                     inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
                     where rd.indicator_id = di.indicator_id
                       and (ids.rsf_program_id = di.subscription_rsf_pfcbl_id or ids.rsf_facility_id = di.subscription_rsf_pfcbl_id))
  )
  delete from p_rsf.rsf_program_facility_indicators pfi
  using deleted_subscriptions ds
  where ds.subscription_rsf_pfcbl_id = pfi.rsf_pfcbl_id
    and ds.indicator_id = pfi.indicator_id
    and pfi.is_auto_subscribed is true;
  */    
  	
	update p_rsf.rsf_data_checks chk
	set check_data_id_is_current = 	exists(select * from p_rsf.rsf_data_current rdc where rdc.data_id = chk.data_id)
	from deleted_rsf_data drd
	where drd.rsf_pfcbl_id = chk.rsf_pfcbl_id
	  and drd.indicator_id = chk.indicator_id
		and chk.check_asof_date >= drd.reporting_asof_date;

 	raise info 'rsf_data_deleted_data_current: set check counts %',
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();
																			 
	return NULL;  								 
	 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_deleted_pfcbl_reporting
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_deleted_pfcbl_reporting"();
CREATE FUNCTION "p_rsf"."rsf_data_deleted_pfcbl_reporting"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN


 if (not exists(select * from deleted_rsf_data))
	then
		return null;
	end if;

  if exists(select * from p_rsf.rsf_pfcbl_reporting rpr
	          where exists(select * from deleted_rsf_data drd
						             where drd.data_id = rpr.created_by_data_id))
  THEN
	
		raise info 'rsf_data_deleted_pfcbl_reporting triggered in %',
		(select clock_timestamp()-msg_time);

		msg_time := clock_timestamp();
		
    
    
  insert into p_rsf.rsf_pfcbl_reporting(rsf_pfcbl_id,
                                        reporting_asof_date,
                                        created_by_data_id,
                                        reporting_indicator_id)
  select 
    rids.rsf_pfcbl_id,
    reporting.reporting_asof_date,
    min(reporting.data_id) as created_by_data_id,
    ind.indicator_id
  from (
    select 
      rd.rsf_pfcbl_id,
      rd.reporting_asof_date,
      rd.data_id as data_id
    from p_rsf.rsf_data rd 
    inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
    where ind.indicator_sys_category = 'entity_reporting'
      and exists(select * from deleted_rsf_data drd
                 where drd.rsf_pfcbl_id = rd.rsf_pfcbl_id
                   and drd.reporting_asof_date = rd.reporting_asof_date)
      and not exists(select * from deleted_rsf_data drd2 where drd2.data_id <> rd.data_id)
  ) as reporting 
  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = reporting.rsf_pfcbl_id
  inner join lateral unnest(array[ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id,ids.rsf_loan_id]) as reported_rsf_pfcbl_id on true
  inner join p_rsf.rsf_pfcbl_ids rids on rids.rsf_pfcbl_id = reported_rsf_pfcbl_id
  inner join p_rsf.indicators ind on ind.data_category = rids.pfcbl_category
                                 and ind.indicator_sys_category = 'entity_reporting'
  group by                                  
    rids.rsf_pfcbl_id,
    reporting.reporting_asof_date,
    ind.indicator_id  
  on conflict(rsf_pfcbl_id,reporting_asof_date) 
  do update
	set created_by_data_id = EXCLUDED.created_by_data_id; 
    
  
  delete from p_rsf.rsf_pfcbl_reporting rpr
  using deleted_rsf_data drd
  where drd.data_id = rpr.created_by_data_id;
    
/*
    
		with revert_reporting as MATERIALIZED (
			select 
			fam.parent_rsf_pfcbl_id as rsf_pfcbl_id,
			fam.parent_pfcbl_category as pfcbl_category,
			reporting.reporting_asof_date,
			min(recreated_by_data_id) as created_by_data_id
		-- All the reporting entities that are deleted by this data_id
		-- Where an entity with that data_id (still) has an entity_reporting entry in rsf_data
		from (select 
						rd.rsf_pfcbl_id,
						rd.reporting_asof_date,
						drd.data_id as deleted_data_id,
						min(rd.data_id) as recreated_by_data_id
					from deleted_rsf_data drd
					inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = drd.rsf_pfcbl_id
																			and rd.reporting_asof_date = drd.reporting_asof_date
					inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
					where ind.indicator_sys_category = 'entity_reporting'
						and rd.data_id <> drd.data_id -- shouldn't be possible since it's deleted
						and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
											 where rpr.created_by_data_id = drd.data_id)
					group by										 
						rd.rsf_pfcbl_id,
						rd.reporting_asof_date,
						drd.data_id								 
				 ) as reporting -- all entities that are reporting in this insert
		 inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = reporting.rsf_pfcbl_id -- all their parents
		 where not exists(select * from p_rsf.rsf_pfcbl_reporting rpr
											where rpr.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
												and rpr.reporting_asof_date = reporting.reporting_asof_date
												and rpr.created_by_data_id = reporting.recreated_by_data_id
												and rpr.created_by_data_id < least(reporting.recreated_by_data_id,reporting.deleted_data_id))
		 group by
		 fam.parent_rsf_pfcbl_id,
		 fam.parent_pfcbl_category,
		 reporting.reporting_asof_date
		)
		-- A plain UPDATE should be suitable.  But just in case some parent entity isn't properly represented, upsert instead
		insert into p_rsf.rsf_pfcbl_reporting(rsf_pfcbl_id,
																					reporting_asof_date,
																					created_by_data_id,
																					reporting_indicator_id)
		select 																				
			rr.rsf_pfcbl_id,
			rr.reporting_asof_date,
			rr.created_by_data_id,
			ind.indicator_id as reporting_indicator_id
		from revert_reporting rr
		inner join p_rsf.indicators ind on ind.data_category = rr.pfcbl_category
		where ind.indicator_sys_category = 'entity_reporting'								
		on conflict(rsf_pfcbl_id,reporting_asof_date)
		do update 
		set created_by_data_id = EXCLUDED.created_by_data_id;

		raise info 'rsf_data_deleted_pfcbl_reporting reverted reporting in %',
		(select clock_timestamp()-msg_time);

*/
		msg_time := clock_timestamp();

/*
		create temp table _temp_unreporting(rsf_pfcbl_id int,
																				reporting_asof_date date,
																				primary key (rsf_pfcbl_id,reporting_asof_date))
		on commit drop;
		
		insert into _temp_unreporting(rsf_pfcbl_id,reporting_asof_date)
		select 
			rpr.rsf_pfcbl_id,
			rpr.reporting_asof_date
		from p_rsf.rsf_pfcbl_reporting rpr
		where exists(select * from deleted_rsf_data drd 
								 where drd.data_id = rpr.created_by_data_id);
									 
		analyze _temp_unreporting;										 

		raise info 'rsf_data_deleted_pfcbl_reporting created _temp_unreporting %',
		(select clock_timestamp()-msg_time);
		msg_time := clock_timestamp();
		
		delete from p_rsf.rsf_data_calculation_evaluations dce								 
		where exists(select * from _temp_unreporting tur
								 where tur.rsf_pfcbl_id = dce.rsf_pfcbl_id
									 and tur.reporting_asof_date = dce.calculation_asof_date);

		raise info 'rsf_data_deleted_pfcbl_reporting deleted from rsf_data_calculation_evaluations in %',
		(select clock_timestamp()-msg_time);
		msg_time := clock_timestamp();

		delete from p_rsf.rsf_data_check_evaluations dce
		where exists(select * from _temp_unreporting tur
								 where tur.rsf_pfcbl_id = dce.rsf_pfcbl_id
									 and tur.reporting_asof_date = dce.check_asof_date);
									 
		raise info 'rsf_data_deleted_pfcbl_reporting deleted from rsf_data_check_evaluations in %',
		(select clock_timestamp()-msg_time);
		msg_time := clock_timestamp();

		raise info 'rsf_data_deleted_pfcbl_reporting deleted from rsf_data_checks in %',
		(select clock_timestamp()-msg_time);
	
		delete from p_rsf.rsf_pfcbl_reporting rpr
		where exists(select * from _temp_unreporting tur
								 where tur.rsf_pfcbl_id = rpr.rsf_pfcbl_id
									 and tur.reporting_asof_date = rpr.reporting_asof_date);		             
	 
	 
	  -- using fk cascade
		--delete from p_rsf.rsf_pfcbl_reporting rpr
		--where exists(select * from deleted_rsf_data drd 
		--						 where drd.data_id = rpr.created_by_data_id);
                 
		drop table _temp_unreporting;
*/    
  END IF;
	
	return NULL;
	 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_id_normalized
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_id_normalized"("input_id" text);
CREATE FUNCTION "p_rsf"."rsf_data_id_normalized"("input_id" text)
  RETURNS TABLE("id_normalized" text) AS $BODY$
begin 
  
	if (input_id ~ '^0+') = true
	then
		input_id := regexp_replace(input_id,'^0+','');
	end if;
	
	if (input_id ~ '#[0-9]+$') = true
	then
		input_id := regexp_replace(input_id,'#[0-9]+$','');	
	end if;
	
	if (input_id ~ '^[0-9]{5,}[A-Za-z]{1,2}$')
	then
		input_id := regexp_replace(input_id,'^([0-9]{5,})[A-Za-z]{1,2}$','\1');
	end if;
	
	-- if it doesn't have any non-permitted values, eg - and _ and | and potential compound ID delimiters
	-- and also, we don't have expectation that client is using letters to designate renewals, eg, loans 1234A and 1234B
	if (input_id ~ '[^[:alnum:]\.:_-]+') = false  
	then 	  
			return query select input_id;
	-- if the length is less than 5, it's probably a funky ID format, eg, A-034, and not a compound ID	
	elseif (char_length(input_id) <= 5)
	then
	  -- then just return the funky ID, truncated for zero-padding
		return query select input_id;
	else 
	  -- BUT, if we do have potential compound ID delimiters
		-- Then, split possible delimiters
		-- And then replace deliberate issuance classifications, eg, #1, #2, #3 with nothing (to get the base ID value)
		-- And then replace tailing letters that might be classicaitions, eg, A and B (to get the base ID value)
		-- And then truncate possible leading zeros
		-- and then don't return partial delimited IDs that are 5 characters or less
		return query
		select normalized_id
		from (
			select regexp_replace(unnest(regexp_split_to_array(input_id,'[^[:alnum:]\.:_-]+')),'^0+','') as normalized_id -- remove zero-padded ids
		) nids
		where char_length(nids.normalized_id) >= 5;
	end if;
	
	return;
	
end $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100
  ROWS 1000;

-- ----------------------------
-- Function structure for rsf_data_inserted_data_current
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_inserted_data_current"();
CREATE FUNCTION "p_rsf"."rsf_data_inserted_data_current"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN
	
	if (not exists(select * from inserted_rsf_data)) then 
		return NULL;
	end if;
	
	/* NO! Because if a user changes a value and the changes it back in the same reporting dataset, then the latest change will 
	   not get saved because the redundancy value will not be current and then the reverted value will not be seen as meaningfully
		 different as it is not in rsf_data_current (but only in rsf_data)

  if (exists(select * from inserted_rsf_data ird
	           where exists(select * from p_rsf.reporting_cohorts rc
						              where rc.reporting_cohort_id = ird.reporting_cohort_id
													  and rc.is_redundancy_cohort = true)))
  then
	  raise info 'REDUNDANCY COHORT REPORTED: SKIPPING';
	  return NULL;
  end if;
	*/										
	
	
	raise notice 'rsf_data_inserted_data_current p_rsf.rsf_data INSERT(%) @%s from start TG_OP=% and trigger_depth=%',
	(select count(*) from inserted_rsf_data),
	(clock_timestamp()-NOW()),
	TG_OP,pg_trigger_depth();
	
	msg_time := clock_timestamp();
 
  
insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
																										indicator_id,
																										formula_id,
																										rsf_program_id,
																										rsf_facility_id,
																										is_subscribed,
																										is_auto_subscribed,
                                                    subscription_comments,
                                                    auto_subscribed_by_reporting_cohort_id)
   select 
    reported.rsf_pfcbl_id,
    reported.indicator_id,
    indf.formula_id,
    ids.rsf_program_id,
    ids.rsf_facility_id,
    true as is_subscribed,
    true as is_auto_subscribed,
    'SYSTEM: Auto subscribed indicator submitted by reporting cohort ' || reported.reporting_cohort_id as subscription_comments,
    reported.reporting_cohort_id
  from 
  (
    select distinct
      coalesce(ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id,
      ird.indicator_id,
      ird.reporting_cohort_id
    from inserted_rsf_data ird
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ird.rsf_pfcbl_id
  ) reported
  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = reported.rsf_pfcbl_id
  left join p_rsf.indicator_formulas indf on indf.indicator_id = reported.indicator_id
	                                       and indf.is_primary_default = true
  where not exists(select * from p_rsf.rsf_setup_indicators pfi 
                   where pfi.rsf_pfcbl_id = reported.rsf_pfcbl_id 
                     and pfi.indicator_id = reported.indicator_id)
  on conflict(rsf_pfcbl_id,indicator_id) -- if it's already there, subscribed or unsubscribed at this entity's level, do nothing.
	do nothing;


	raise info 'rsf_data_inserted_data_current: (1) completed insert into p_rsf.rsf_setup_indicators in %',
  (select clock_timestamp()-msg_time); msg_time := clock_timestamp();
 
	
	--raise notice 'rsf_data_inserted_data_current p_rsf.rsf_data subscriptions done!';
	
  create temp table _temp_current_data(data_id int,
	                                     rsf_pfcbl_id int,
																			 indicator_id int,
																			 reporting_asof_date date,
																			 reporting_cohort_id int,
																			 data_time timestamptz,
																			 data_value text,
																			 data_unit text,
																			 data_unit_data_id int,
																			 data_reporting_flags int,
																			 data_type text,
																			 indicator_sys_category text,
																			 data_category text)
  on commit drop;
																			 
  insert into _temp_current_data(data_id,
	                               rsf_pfcbl_id,
																 indicator_id,
																 reporting_asof_date,
																 reporting_cohort_id,
																 data_time,
																 data_value,
																 data_unit,
																 data_unit_data_id,
																 data_reporting_flags,
																 data_type,
																 indicator_sys_category,
																 data_category)
  select ird.data_id,
				 ird.rsf_pfcbl_id,
				 ird.indicator_id,
				 ird.reporting_asof_date,
				 ird.reporting_cohort_id,
				 now()::timestamptz as data_time,
				 ird.data_value,
				 ird.data_unit,
				 NULL::int,
				 0::int,
				 ind.data_type,
				 ind.indicator_sys_category,
				 ind.data_category
	from inserted_rsf_data ird
	inner join p_rsf.indicators ind on ind.indicator_id = ird.indicator_id;
	
										 
	alter table _temp_current_data add primary key(rsf_pfcbl_id,indicator_id,reporting_asof_date);
	create index _temp_current_data_isc_idx on _temp_current_data(indicator_sys_category);
	
	analyze _temp_current_data;
	
  raise info 'rsf_data_inserted_data_current: (2) completed creating _temp_current_data and indexes in %',
  (select clock_timestamp()-msg_time); msg_time := clock_timestamp();

	perform p_rsf.function_rsf_data_current_update();
	
	
  raise info 'rsf_data_inserted_data_current: (3) completed perform p_rsf.function_rsf_data_current_update(); in %',
  (select clock_timestamp()-msg_time); msg_time := clock_timestamp();
	--raise warning 'TESTING contents of _temp_current_data: %',		
	--(select json_agg(row_to_json(js)) from (select * from _temp_current_data) js);
						
	

	create temp table _temp_loan_issuances(rsf_pfcbl_id int,
																	 data_id int,
																	 series_id text);
																	 
  with issuances as MATERIALIZED (
		select 
			tcd.rsf_pfcbl_id,
			--rdc.indicator_id,
			tcd.data_id,
			--siblings.parent_rsf_pfcbl_id,
			siblings.matched_rsf_pfcbl_id,
			siblings.matched_data_id,
			siblings.series_id
		from _temp_current_data tcd
		left join lateral p_rsf.function_rsf_loan_issuance_series(input_rsf_pfcbl_id => tcd.rsf_pfcbl_id,
																															input_id_value => tcd.data_value,
																															input_id_indicator_id => tcd.indicator_id) as siblings on true 
		where tcd.indicator_sys_category = 'id' 
			and tcd.data_category = 'loan'
			-- this is the currentest ID value submitted (ie, if they're doing an historic backfill, old ID updates won't effect).
			and not exists(select * from p_rsf.rsf_data_current futures
										 where futures.rsf_pfcbl_id = tcd.rsf_pfcbl_id
											 and futures.indicator_id = tcd.indicator_id
											 and futures.reporting_asof_date > tcd.reporting_asof_date)		
	)
	insert into _temp_loan_issuances(rsf_pfcbl_id,
	                                 data_id,
																	 series_id)
	select 
		lis.rsf_pfcbl_id,																 
		min(lis.data_id) as data_id,
		min(lis.series_id) as series_id
	from (
		select 
		iss.rsf_pfcbl_id,
		iss.data_id,
		iss.series_id
		from issuances iss

		union

		select 
		iss.matched_rsf_pfcbl_id,
		iss.matched_data_id,
		iss.series_id
		from issuances iss
		where series_id is not NULL
	) lis
	group by
		lis.rsf_pfcbl_id;
	
	alter table _temp_loan_issuances add primary key(rsf_pfcbl_id);
	analyze _temp_loan_issuances;
	
	delete from p_rsf.rsf_loan_issuance_series lis
	where exists(select * from _temp_loan_issuances tli
	             where tli.series_id is NULL
							   and tli.rsf_pfcbl_id = lis.rsf_pfcbl_id);

  delete from _temp_loan_issuances tli 
	where tli.series_id is NULL;
	
	insert into p_rsf.rsf_loan_issuance_series(rsf_pfcbl_id,
																							loan_issuance_series_id,
																							loan_issuance_series_rank,
																							id_value_data_id)
  select 
		tli.rsf_pfcbl_id,
		tli.series_id,
		dense_rank() over(partition by tli.series_id order by tli.rsf_pfcbl_id) as series_rank,
		tli.data_id as id_value_data_id
  from _temp_loan_issuances tli		
	on conflict(rsf_pfcbl_id)
	do update
	set loan_issuance_series_id = EXCLUDED.loan_issuance_series_id,
	    loan_issuance_series_rank = EXCLUDED.loan_issuance_series_rank,
			id_value_data_id = EXCLUDED.id_value_data_id;					 
						
  drop table _temp_loan_issuances;

	raise info 'rsf_data_inserted_data_current: (4) completed loan_issuances stuff in %',
  (select clock_timestamp()-msg_time); msg_time := clock_timestamp();
	
	
	update p_rsf.rsf_data_checks chk
	set check_data_id_is_current = 	exists(select * from p_rsf.rsf_data_current rdc where rdc.data_id = chk.data_id)
	from inserted_rsf_data ird
	where ird.rsf_pfcbl_id = chk.rsf_pfcbl_id
	  and ird.indicator_id = chk.indicator_id
		and chk.check_asof_date >= ird.reporting_asof_date;


	raise info 'rsf_data_inserted_data_current: (5) completed setting rsf_data_checks check_data_id_is_current in %',
  (select clock_timestamp()-msg_time); msg_time := clock_timestamp();
  
	/*
	raise warning 'TESTING contents of trigger_rsf_data_3_inserted_data_current entity_local_currency_unit: %',
	(select array_agg(distinct lcu_unit_data_id || '-' || for_rsf_pfcbl_id || '-' || reporting_asof_date) from p_rsf.rsf_data_current_lcu lcu where 
	 not exists(select * from p_rsf.rsf_data_current rdc where rdc.data_id = lcu_unit_data_id));

	raise warning 'TESTING contents of trigger_rsf_data_3_inserted_data_current entity_local_currency_unit: %',
	(select count(*) from _temp_current_data tcd where 
	 not exists(select * from p_rsf.rsf_data_current rdc where rdc.data_id = tcd.data_id));
*/
	 drop table _temp_current_data;

	raise info 'rsf_data_inserted_data_current: (6) drop table and COMPLETED in %',
  (select clock_timestamp()-msg_time); msg_time := clock_timestamp();
	 
	return NULL;
		
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_inserted_data_integrity
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_inserted_data_integrity"();
CREATE FUNCTION "p_rsf"."rsf_data_inserted_data_integrity"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
declare cohort_counts int default null;
BEGIN

  if exists(select * from inserted_rsf_data tud
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tud.rsf_pfcbl_id
						inner join p_rsf.indicators ind on ind.indicator_id = tud.indicator_id
						where ids.pfcbl_category <> ind.data_category)
  then
		raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity(): entity pfcbl_category and indicator data_category misalignment: %',
		
		(select left(json_agg(row_to_json(js))::text,900)
		from (
		select ids.rsf_pfcbl_id,ids.pfcbl_category,ind.indicator_name,ind.indicator_id
		from inserted_rsf_data tud
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tud.rsf_pfcbl_id
						inner join p_rsf.indicators ind on ind.indicator_id = tud.indicator_id
						where ids.pfcbl_category <> ind.data_category) js);
	end if;
	
	if exists(select * from inserted_rsf_data tud 
              inner join p_rsf.indicators ind on ind.indicator_id = tud.indicator_id
							where tud.data_unit is NULL and ind.data_unit is not null)
  then
	  
		raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity(): indicator data_unit is not NULL but data_unit is NULL: %',
		(select left(json_agg(row_to_json(js))::text,900)
		from (
		select tud.rsf_pfcbl_id,ind.indicator_name,ind.indicator_id,tud.data_unit as inserted_data_unit,ind.data_unit as default_data_unit
		from inserted_rsf_data tud
    inner join p_rsf.indicators ind on ind.indicator_id = tud.indicator_id
		where tud.data_unit is NULL and ind.data_unit is not null) js);
						

  end if;
			
	if exists(select * from inserted_rsf_data tud
              where tud.data_unit is distinct from NULLIF(upper(trim(tud.data_unit)),'')
							   or tud.data_value is distinct from NULLIF(trim(tud.data_value),''))
   then								 
	  raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity() where data_unit or data_value is not distinct from NULLIF(TRIM(),"")\n\n%',
    (select left(json_agg(row_to_json(js))::text,900)
		from (
		select sn.pfcbl_name,tud.rsf_pfcbl_id,ind.indicator_name,ind.indicator_id,tud.data_unit,tud.data_value
		from inserted_rsf_data tud
    inner join p_rsf.indicators ind on ind.indicator_id = tud.indicator_id
    left join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = tud.rsf_pfcbl_id
		where tud.data_unit is distinct from NULLIF(upper(trim(tud.data_unit)),'')
							   or tud.data_value is distinct from NULLIF(trim(tud.data_value),'')
          ) js);
	end if;
	
	if exists(select * from inserted_rsf_data tud
              inner join p_rsf.indicators ind on ind.indicator_id = tud.indicator_id
							where ind.is_static_nonreporting = true)
  then							
	  raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity(): is_static_non_reporting indicator reported';
	end if;
	
	if exists(select * from inserted_rsf_data tud
            inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = tud.reporting_cohort_id
						inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tud.rsf_pfcbl_id
						where rc.reporting_rsf_pfcbl_id <> coalesce(ids.rsf_client_id,ids.rsf_facility_id,ids.rsf_program_id)
              and rc.reporting_type > 0)			
  then 																	 
		raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity(): rc.reporting_rsf_pfcbl_id must report client+ data under rsf_client_id, facility data under rsf_facility_id and program data under rsf_program_id (unless cohort reporting type=0, system setup data)';
	end if;
/*
  if (exists(select * from p_rsf.reporting_cohorts rc
						 where exists(select * from inserted_rsf_data ird
									        where ird.reporting_cohort_id = rc.reporting_cohort_id)
							 and rc.parent_reporting_cohort_id is NULL))
  then
		raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity(): Top-level parent reporting cohorts (%) may not report data: report data under child cohorts',
		(select array_agg(distinct ird.reporting_cohort_id) from inserted_rsf_data ird);
	
	end if;
*/	
	
  -- generally only one cohort is inserted at a time.  But really we want to ensure that only one TYPE of cohort
	-- is inserted at a time: because is_calculated_cohort will validate calculations that are awaiting evaluation in rsf_data_calculation_evaluations
	-- that we want to ensure for example that both calculated data and user reported data aren't simultaneously entered.
	-- db_add_update_data_system will assign a reporting_cohort for first-available slot.  And tehrefore, may insert two (or more) calculated cohort IDs 
	-- within the same statement.
  if (select count(distinct ird.reporting_cohort_id) from inserted_rsf_data ird) > 1
	then
	  
    if (exists(select true
               from p_rsf.reporting_cohorts rc
               where exists(select * from inserted_rsf_data ird
                            where ird.reporting_cohort_id = rc.reporting_cohort_id)
               group by rc.reporting_type
               having count(distinct rc.reporting_type) > 1))
    then                
			raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity(): Only one reporting cohort TYPE can insert data at a time but received data for cohorts=%',
			(select array_agg(distinct ird.reporting_cohort_id) from inserted_rsf_data ird);
    end if;
  end if;
  
  if exists(select true
            from inserted_rsf_data ird
            group by
              ird.rsf_pfcbl_id,
              ird.indicator_id,
              ird.reporting_asof_date
            having count(*) > 1)
  then
    
    raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity(): Only one rsf_pfcbl_id,indicator_id,reporting_asof_date value set can be inserted per statement, but repeats exist for cohorts=%',
    (select array_agg(distinct ird.reporting_cohort_id) from inserted_rsf_data ird);
  
  end if;
			

	RETURN NULL;

END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_inserted_data_unit_lcu
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_inserted_data_unit_lcu"();
CREATE FUNCTION "p_rsf"."rsf_data_inserted_data_unit_lcu"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
declare formula_calculation_data_unit text;
declare formula_calculation_data_type text;
BEGIN

  -- All units are upper case
	if (NEW.data_unit ~ '[a-z]') 
  then 
		NEW.data_unit := upper(NEW.data_unit);
	end if;
	
  -- For example, if the entity's LCU value is XOF
	-- And the user uploads a currency indicator whose unit is XOF, then we should not interpret this as a defined and fixed currency unit for 
	-- that value; rather, we should interpret it as a generic LCU value.
	-- This is an issue if a user uploads 1000LCU and rsf_data_current inserts this as 1000XOF
	-- And this is a calculated value, and the calculator re-calculates it and finds that it is 1000XOF and compared if the change is meaningfully
	-- different and compares 1000LCU to 1000XOF and considers it a change--which it isn't.
	-- Rarely, also an issue for a changed currency regime
	
	
  if (NEW.data_unit is NOT NULL
	    AND
			NEW.data_unit <> 'LCU'
			AND
			NEW.data_unit ~ '[A-Z]{3}')
  then
	  if (exists(select * from p_rsf.indicators ind
		           where ind.indicator_id = NEW.indicator_id
							 and ind.data_unit = 'LCU'))
	  then 
	 
	    if (select lcu.data_unit_value
					from p_rsf.rsf_data_current_lcu lcu
					where lcu.for_rsf_pfcbl_id = NEW.rsf_pfcbl_id
						and lcu.reporting_asof_date <= NEW.reporting_asof_date
					order by lcu.reporting_asof_date desc
					limit 1) is not distinct from NEW.data_unit
			then
			  NEW.data_unit := 'LCU';
			end if;			
		end if;
	end if;
	
	-- this is not at all efficient for each data insert. But should be fast on the index and these data types are very rare.	
	RETURN NEW;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_inserted_pfcbl_reporting
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_inserted_pfcbl_reporting"();
CREATE FUNCTION "p_rsf"."rsf_data_inserted_pfcbl_reporting"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

  if (not exists(select * from inserted_rsf_data))
	then
		return null;
	end if;	

	insert into p_rsf.rsf_pfcbl_reporting(rsf_pfcbl_id,
																				reporting_asof_date,
																				created_by_data_id,
																				reporting_indicator_id)
  select 
    rids.rsf_pfcbl_id,
    reporting.reporting_asof_date,
    reporting.data_id,
    ind.indicator_id
  from (select 
				  ird.rsf_pfcbl_id,
					ird.reporting_asof_date,
					ird.data_id
				from inserted_rsf_data ird
				inner join p_rsf.indicators ind on ind.indicator_id = ird.indicator_id
				where ind.indicator_sys_category = 'entity_reporting'
				  and not exists(select * from p_rsf.rsf_pfcbl_reporting rpr
					               where rpr.rsf_pfcbl_id = ird.rsf_pfcbl_id
												   and rpr.reporting_asof_date = ird.reporting_asof_date)
	) as reporting -- all entities that are reporting in this insert
  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = reporting.rsf_pfcbl_id
  inner join lateral unnest(array[ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id,ids.rsf_loan_id]) as reported_rsf_pfcbl_id on true
  inner join p_rsf.rsf_pfcbl_ids rids on rids.rsf_pfcbl_id = reported_rsf_pfcbl_id
  inner join p_rsf.indicators ind on ind.data_category = rids.pfcbl_category
                                 and ind.indicator_sys_category = 'entity_reporting'
  on conflict do nothing;
  
  /*
	insert into p_rsf.rsf_pfcbl_reporting(rsf_pfcbl_id,
																				reporting_asof_date,
																				created_by_data_id,
																				reporting_indicator_id)

	select 
		fam.parent_rsf_pfcbl_id as rsf_pfcbl_id,
		reporting.reporting_asof_date,
		min(data_id) as created_by_data_id,
		pind.indicator_id as reporting_indicator_id
	from (select 
				  ird.rsf_pfcbl_id,
					ird.reporting_asof_date,
					ird.reporting_cohort_id,
					ird.data_id
				from inserted_rsf_data ird
				inner join p_rsf.indicators ind on ind.indicator_id = ird.indicator_id
				where ind.indicator_sys_category = 'entity_reporting'
				  and not exists(select * from p_rsf.rsf_pfcbl_reporting rpr
					               where rpr.rsf_pfcbl_id = ird.rsf_pfcbl_id
												   and rpr.reporting_asof_date = ird.reporting_asof_date)
			 ) as reporting -- all entities that are reporting in this insert
	 inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = reporting.rsf_pfcbl_id -- all their parents
	 inner join p_rsf.indicators pind on pind.data_category = fam.parent_pfcbl_category
	                                 and pind.indicator_sys_category = 'entity_reporting'
	 where not exists(select * from p_rsf.rsf_pfcbl_reporting rpr
					          where rpr.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
											and rpr.reporting_asof_date = reporting.reporting_asof_date)
	 group by
	 fam.parent_rsf_pfcbl_id,
	 reporting.reporting_asof_date,
	 pind.indicator_id
	 on conflict do nothing;
*/

	 return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_modified_calculations
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_modified_calculations"();
CREATE FUNCTION "p_rsf"."rsf_data_modified_calculations"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
DECLARE trigger_is_calculated_cohort bool default false;
BEGIN

  if (not exists(select * from modified_data))
	then 
		return null;
	end if;
	
  msg_time := clock_timestamp();
  
	if (TG_OP)='INSERT'
  then
  
    --limit 1 because insert/update only allows one reporting cohort per statement
    --whereas delete can delete cohorts en-mass.
    select rc.is_calculated_cohort
    into trigger_is_calculated_cohort
    from p_rsf.reporting_cohorts rc
    where rc.reporting_cohort_id = (select md.reporting_cohort_id from modified_data md limit 1);   
   
  end if;  
 
    
  create temp table _modified_data as
  select 
  data_id,
  rsf_pfcbl_id,
  indicator_id,
  reporting_asof_date
  from modified_data;
  
  perform p_rsf.function_evaluate_calculations_using_modified_data(event_type => lower(TG_OP),
                                                                   event_is_calculated_cohort => trigger_is_calculated_cohort);
  

	raise info 'rsf_data_current_%_calculations(%) calculated=% in %',
	lower(TG_OP),
	(select count(*) from modified_data),
  trigger_is_calculated_cohort,
  (clock_timestamp()-msg_time);

  drop table _modified_data;
  --drop table _calculate;
	return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_modified_checks
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_modified_checks"();
CREATE FUNCTION "p_rsf"."rsf_data_modified_checks"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

--raise warning 'skipping rsf_data_current_modified_calculations';
--return null;

  if (not exists(select * from modified_data))
	then 
		return null;
	end if;
	
	
  /*
  msg_time := clock_timestamp();
	raise info 'rsf_data_current_%_checks(%)',
	(upper(TG_OP)),
	(select count(*) from modified_data);
	*/															 
  create temp table _check(rsf_pfcbl_id int,
													 check_asof_date date,
													 check_formula_id int,
													 primary key (rsf_pfcbl_id,check_asof_date,check_formula_id))
  on commit drop;


	insert into _check(rsf_pfcbl_id,
										 check_asof_date,
										 check_formula_id)
	select
			pids.to_check_rsf_pfcbl_id,			
			mcd.reporting_asof_date,
			pids.to_check_formula_id
		from modified_data mcd
		inner join p_rsf.compute_check_from_parameter_rsf_pfcbl_id pids	on pids.from_parameter_rsf_pfcbl_id = mcd.rsf_pfcbl_id
																											             and pids.from_parameter_indicator_id = mcd.indicator_id
		where pids.is_calculation_trigger_parameter = true	-- ie, its not used for generating messages
	on conflict 
	do nothing;


/*
	raise info 'rsf_data_current_%_checks parameters triggers % in %',
	(upper(TG_OP)),
	(select count(*) from _check),
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();
*/	
	with new_reporting as MATERIALIZED (
		select distinct
			rpr.rsf_pfcbl_id,
			rpr.reporting_asof_date,
			ind.indicator_id
		from modified_data mcd
		inner join p_rsf.rsf_pfcbl_reporting rpr on rpr.created_by_data_id = mcd.data_id
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rpr.rsf_pfcbl_id
		inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
		where ind.indicator_sys_category = 'entity_reporting'
	)
	insert into _check(rsf_pfcbl_id,
										 check_asof_date,
										 check_formula_id)				
	select 
		pids.to_check_rsf_pfcbl_id,
		nr.reporting_asof_date as check_asof_date,
		pids.to_check_formula_id
	from new_reporting nr
	inner join p_rsf.compute_check_from_parameter_rsf_pfcbl_id pids on pids.from_parameter_rsf_pfcbl_id = nr.rsf_pfcbl_id
																										             and pids.from_parameter_indicator_id = nr.indicator_id
	where pids.is_calculation_trigger_parameter = true	
  on conflict
	do nothing;

	analyze _check;
/*
	raise info 'rsf_data_current_%_checks parameters where parameter_trigger_by_reporting=true % in %',
	(upper(TG_OP)),
	(select count(*) from _check),
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();
*/	
	
	-- TRIGGERED BY PRE-EXISTING PARAMETERS THAT A PARENT ENTITY REPORTED BEFORE THIS ENTITY'S FIRST REPORTING	

  with existing_parameters as MATERIALIZED (
		select 
			pids.to_check_rsf_pfcbl_id,
			parents.reporting_asof_date,
			pids.to_check_formula_id
		from (		
			select distinct
				ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id, -- newly reported ID
				rpr.reporting_asof_date                  -- for newly reported data
			from p_rsf.rsf_pfcbl_reporting rpr
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = rpr.rsf_pfcbl_id
                                                       and ft.pfcbl_hierarchy = 'parent' -- parent entity (ie, not itself)
			where exists(select * from modified_data mcd
									 where mcd.data_id = rpr.created_by_data_id)
									 
      union all 
			
			select distinct 0,reporting_asof_date from modified_data 									 
		) as parents 
		-- all parent data already reported this period
		inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = parents.rsf_pfcbl_id																				
																				 and rdc.reporting_asof_date = parents.reporting_asof_date
		inner join p_rsf.compute_check_from_parameter_rsf_pfcbl_id pids on pids.from_parameter_rsf_pfcbl_id = rdc.rsf_pfcbl_id
																											             and pids.from_parameter_indicator_id = rdc.indicator_id
		where pids.is_calculation_trigger_parameter = true
  )																													 
	insert into _check(rsf_pfcbl_id,
										 check_asof_date,
										 check_formula_id)		
  select
		ep.to_check_rsf_pfcbl_id,
		ep.reporting_asof_date,
		ep.to_check_formula_id
	from existing_parameters ep												 
  -- My parent-level parameter has triggered my need to (re)calculate
	where exists(select * from p_rsf.rsf_pfcbl_reporting rpr
	             inner join modified_data mcd on mcd.data_id = rpr.created_by_data_id
	             where rpr.rsf_pfcbl_id = ep.to_check_rsf_pfcbl_id
							   and rpr.reporting_asof_date = ep.reporting_asof_date)
  on conflict do nothing;
					
	
	analyze _check;
	/*
	raise info 'rsf_data_current_%_checks after deletes has % in %',
	(upper(TG_OP)),
	(select count(*) from _check),
	(select clock_timestamp()-msg_time);
	msg_time := clock_timestamp();
  */
	

-- currently only calculations triggered by parameters: but a new parameter coming in "now" could affect all calculations that 
	-- use this parameter on up into the future.
		insert into p_rsf.rsf_data_check_evaluations(rsf_pfcbl_id,
																								 check_asof_date,
																								 check_formula_id)
		select 
			chk.rsf_pfcbl_id,																				 
			rpr.reporting_asof_date as check_asof_date,
			chk.check_formula_id
		from _check chk		
		inner join p_rsf.rsf_pfcbl_reporting rpr on rpr.rsf_pfcbl_id = chk.rsf_pfcbl_id                   -- if we didn't report, we don't check
		inner join p_rsf.view_rsf_setup_check_subscriptions scs on scs.rsf_pfcbl_id = rpr.rsf_pfcbl_id
		                                                       and scs.check_formula_id = chk.check_formula_id
		where rpr.reporting_asof_date >= chk.check_asof_date
		  and scs.is_subscribed is true
		on conflict(rsf_pfcbl_id,check_asof_date,check_formula_id)
    do nothing;
/*
	raise info 'rsf_data_current_%_checks inserted and updated in %',
	(upper(TG_OP)),
	(select clock_timestamp()-msg_time);
*/
  drop table _check;
	return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_modified_sys_flags
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_modified_sys_flags"();
CREATE FUNCTION "p_rsf"."rsf_data_modified_sys_flags"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN


if  (coalesce(NEW.data_sys_flags,0) & 4) = 4 
   or
	 (coalesce(OLD.data_sys_flags,0) & 4) = 4 then

  -- manual calculation flag is only relevant for calculated indicators
	if not exists(select * from p_rsf.view_rsf_setup_indicator_subscriptions sis
	              where sis.rsf_pfcbl_id = NEW.rsf_pfcbl_id
								  and sis.indicator_id = NEW.indicator_id
									and sis.is_calculated = true) 
	then
		raise exception 'Failed to set data SYS FLAGS for MANUAL CALCULATION [flag=4] because indicator is classified as non-calculated: rsf_pfcbl_id=% indicator_id=%',new.rsf_pfcbl_id,new.indicator_id;
	end if;
	
	with sys_calculations as (
		select		
			rd.data_id
		from p_rsf.rsf_data rd
		inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
		where rd.rsf_pfcbl_id = NEW.rsf_pfcbl_id
		  and rd.indicator_id = NEW.indicator_id
			and rd.reporting_asof_date = NEW.reporting_asof_date -- calculation overwrites can ONLY happen in same reporting period
			and rd.data_id > NEW.data_id -- overwrite inserts (and hence data_id) will always be greater than reported insert data_ids
			and rc.is_calculated_cohort = true -- will not affected reported data!
	)
	delete from p_rsf.rsf_data rd
	using sys_calculations sc
	where sc.data_id = rd.data_id;	
	
	insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
																										 indicator_id,
																										 calculation_asof_date)
	select 
		rpr.rsf_pfcbl_id,
		NEW.indicator_id,
		rpr.reporting_asof_date
	from p_rsf.rsf_pfcbl_reporting rpr
	where rpr.rsf_pfcbl_id = NEW.rsf_pfcbl_id
	  and rpr.reporting_asof_date >= NEW.reporting_asof_date
	on conflict do nothing;
	
end if;


	
return NEW;
	 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_value_unit
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_value_unit"("v_data_value" text, "v_data_unit" text);
CREATE FUNCTION "p_rsf"."rsf_data_value_unit"("v_data_value" text, "v_data_unit" text)
  RETURNS "pg_catalog"."text" AS $BODY$
begin
																										 
return 
	case when v_data_value is NULL and v_data_unit is NULL then '{NOTHING}'
			 when v_data_value is NULL and v_data_unit is NOT NULL then v_data_unit
			 when v_data_value is NOT NULL and v_data_unit is NULL then v_data_value
			 when v_data_value is NOT NULL and v_data_unit is NOT NULL then v_data_value || ' ' || v_data_unit
	end;
end;
$BODY$
  LANGUAGE plpgsql IMMUTABLE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_loan_issuance_series_obsolete
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_loan_issuance_series_obsolete"();
CREATE FUNCTION "p_rsf"."rsf_loan_issuance_series_obsolete"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

	if not exists(select * from deleted) then
		RETURN NULL;
	end if;

	with obsolete_issuances as MATERIALIZED (	
			select 
				lis.loan_issuance_series_id
			from p_rsf.rsf_loan_issuance_series lis
			where exists(select * from deleted
									 where deleted.loan_issuance_series_id = lis.loan_issuance_series_id)
			group by lis.loan_issuance_series_id
			having count(*) = 1
	)
	delete from p_rsf.rsf_loan_issuance_series lis
	where exists(select * from obsolete_issuances oi
							 where oi.loan_issuance_series_id = lis.loan_issuance_series_id);
 
	return null;				 
END; 
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_pfcbl_check_recalculate
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_pfcbl_check_recalculate"("v_rsf_pfcbl_id" int4, "v_check_formula_id" int4);
CREATE FUNCTION "p_rsf"."rsf_pfcbl_check_recalculate"("v_rsf_pfcbl_id" int4, "v_check_formula_id" int4)
  RETURNS "pg_catalog"."bool" AS $BODY$
BEGIN

if (v_rsf_pfcbl_id is null or v_check_formula_id is null) then
  return false;
end if;

insert into p_rsf.rsf_data_check_evaluations(rsf_pfcbl_id,check_formula_id,check_asof_date)
select 
rdc.rsf_pfcbl_id,
cfp.check_formula_id,
rdc.reporting_asof_date
from p_rsf.indicator_check_formula_parameters cfp
inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_pfcbl_category = cfp.parameter_pfcbl_category
                                                 and ft.to_pfcbl_category = cfp.for_pfcbl_category
inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                     and rdc.indicator_id = cfp.parameter_indicator_id                                                 
where cfp.check_formula_id = v_check_formula_id
  and rdc.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                             from p_rsf.view_rsf_pfcbl_id_family_tree ft
                             where ft.from_rsf_pfcbl_id = v_rsf_pfcbl_id::int)
on conflict do nothing;
     
return true;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_pfcbl_generate_reporting_dates
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_pfcbl_generate_reporting_dates"("v_rsf_pfcbl_id" int4, "v_until_date" date);
CREATE FUNCTION "p_rsf"."rsf_pfcbl_generate_reporting_dates"("v_rsf_pfcbl_id" int4, "v_until_date" date=(now())::date)
  RETURNS TABLE("rsf_pfcbl_id" int4, "valid_reporting_date" date, "reporting_sequence_rank" int4) AS $BODY$
begin 							

  return query							
  SELECT 
    ids.rsf_pfcbl_id,
    (dates.valid_reporting_date - ('1 day'::interval))::date as valid_reporting_date,
    row_number() OVER (PARTITION BY ids.rsf_pfcbl_id 
                       ORDER BY (dates.valid_reporting_date - '1 day'::interval)::date)::int AS reporting_sequence_rank
  FROM p_rsf.rsf_pfcbl_ids ids
  INNER JOIN LATERAL (select * 
                      from generate_series(
                        date_trunc('quarter'::text,(ids.created_in_reporting_asof_date::date)::timestamp with time zone),
                        least(ids.deactivated_in_reporting_asof_date::date,
                              greatest(now()::date,v_until_date::date))::timestamp with time zone + '3 mons'::interval,
                        '3 mons'::interval) as valid_reporting_date) dates on dates.valid_reporting_date >= ids.created_in_reporting_asof_date::date
  where ids.rsf_pfcbl_id = v_rsf_pfcbl_id;

end; 
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100
  ROWS 1000;

-- ----------------------------
-- Function structure for rsf_pfcbl_indicator_recalculate
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_pfcbl_indicator_recalculate"("v_rsf_pfcbl_id" int4, "v_formula_id" int4);
CREATE FUNCTION "p_rsf"."rsf_pfcbl_indicator_recalculate"("v_rsf_pfcbl_id" int4, "v_formula_id" int4)
  RETURNS "pg_catalog"."bool" AS $BODY$
  DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

if (v_rsf_pfcbl_id is null or v_formula_id is null) then
  return false;
end if;

/*
if program is asked to calculate all its children with this formula, this will fail.  
if (not exists(select * from p_rsf.view_rsf_setup_indicator_subscriptions sis
                where sis.rsf_pfcbl_id = v_rsf_pfcbl_id
                  and sis.formula_id = v_formula_id
                  and sis.is_subscribed is true))
 then
  raise exception 'rsf_pfcbl_indicator_recalculate() re-calculate unsubscribed formula: rsf_pfcbl_id=% formula_id=%',
  v_rsf_pfcbl_id,v_formula_id;
 
 end if;
*/
raise notice 'rsf_pfcbl_indicator_recalculate() for rsf_pfcbl_id=% formula_id=% and trigger_depth=%',
	v_rsf_pfcbl_id,
	v_formula_id,
	pg_trigger_depth();


delete from p_rsf.rsf_data_calculation_evaluations dce
using (
  select 
  ind.indicator_id,
  ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
  from p_rsf.indicator_formulas indf
  inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
  inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = v_rsf_pfcbl_id
                                                   and ft.to_pfcbl_category = ind.data_category
  where indf.formula_id = v_formula_id
) remove
where remove.rsf_pfcbl_id = dce.rsf_pfcbl_id
  and remove.indicator_id = dce.indicator_id;



with calc_as_param as (
  select ifp.formula_id,ifp.indicator_id,ifp.parameter_pfcbl_category,ifp.parameter_indicator_id
  from p_rsf.indicator_formula_parameters ifp 
  where ifp.formula_id = v_formula_id
  
  union
  
  -- to enforce calculation to trigger itself where reported in rsf_data_current, below
  select ifp.formula_id,ifp.indicator_id,ifp.calculate_pfcbl_category as parameter_pfcbl_category,ifp.indicator_id as parameter_indicator_id
  from p_rsf.indicator_formula_parameters ifp
  where ifp.formula_id = v_formula_id
),
calcs as materialized (
  select 
    sis.rsf_pfcbl_id,
    calc_as_param.*
  from p_rsf.view_rsf_setup_indicator_subscriptions sis,calc_as_param
  where sis.formula_id = v_formula_id
    and sis.is_subscribed is true
    and sis.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id 
                               from p_rsf.view_rsf_pfcbl_id_family_tree ft
                               where ft.from_rsf_pfcbl_id = v_rsf_pfcbl_id::int
                                 and ft.to_pfcbl_category = (select ind.data_category
                                                             from p_rsf.indicators ind
                                                             inner join p_rsf.indicator_formulas indf on indf.indicator_id = ind.indicator_id
                                                             where indf.formula_id = v_formula_id))

)
insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date)
select distinct
calcs.rsf_pfcbl_id,
calcs.indicator_id,
rdc.reporting_asof_date 
from calcs
inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = calcs.rsf_pfcbl_id
inner join lateral (select max(rpr.reporting_asof_date) as reporting_asof_date
                    from p_rsf.rsf_pfcbl_reporting rpr
                    where rpr.rsf_pfcbl_id = calcs.rsf_pfcbl_id) as reporting on true
inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = calcs.rsf_pfcbl_id
                                                 and ft.to_pfcbl_category = calcs.parameter_pfcbl_category
inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                     and rdc.indicator_id = calcs.parameter_indicator_id 
where rdc.reporting_asof_date >= ids.created_in_reporting_asof_date            
  and rdc.reporting_asof_date <= reporting.reporting_asof_date                                     
on conflict do nothing;                                       
                                       
/*
with calc_as_param as (
  select ifp.formula_id,ifp.indicator_id,ifp.parameter_pfcbl_category,ifp.calculate_pfcbl_category,ifp.parameter_indicator_id
  from p_rsf.indicator_formula_parameters ifp 
  where ifp.formula_id = v_formula_id
  
  union
  
  -- to enforce calculation to trigger itself where reported in rsf_data_current, below
  select ifp.formula_id,ifp.indicator_id,ifp.calculate_pfcbl_category as parameter_pfcbl_category,ifp.calculate_pfcbl_category,ifp.indicator_id as parameter_indicator_id
  from p_rsf.indicator_formula_parameters ifp
  where ifp.formula_id = v_formula_id
),
subs as materialized (
  select 
    sis.rsf_pfcbl_id,
    sis.indicator_id,
    sis.formula_id
  from p_rsf.view_rsf_setup_indicator_subscriptions sis
  where sis.formula_id = v_formula_id
    and sis.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                             from p_rsf.view_rsf_pfcbl_id_family_tree ft
                             where ft.from_rsf_pfcbl_id = v_rsf_pfcbl_id::int)
)
insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date)
select distinct
rdc.rsf_pfcbl_id,
cap.indicator_id,
rdc.reporting_asof_date
from subs
inner join calc_as_param cap on cap.formula_id = subs.formula_id
inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_pfcbl_category = cap.parameter_pfcbl_category
                                                 and ft.to_pfcbl_category = cap.calculate_pfcbl_category
inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                     and rdc.indicator_id = cap.parameter_indicator_id                                           
on conflict do nothing;
*/
            /*                 
raise notice 'rsf_pfcbl_indicator_recalculate(%) for rsf_pfcbl_id=% indicator_id=% and trigger_depth=%',
  (select clock_timestamp()-msg_time),
	v_rsf_pfcbl_id,
	v_formula_id,
	pg_trigger_depth();
	*/
  
  
-- parameters that trigger this calculation 
/*
with calculations as materialized (
	select distinct
	tip.to_calculate_rsf_pfcbl_id,
	tip.to_calculate_indicator_id,
	tip.to_calculate_formula_id,
	tip.reporting_asof_date
	from p_rsf.compute_calculation_triggered_by_parameter tip
	where tip.from_parameter_pfcbl_id = any (select ft.to_family_rsf_pfcbl_id
																					 from p_rsf.view_rsf_pfcbl_id_family_tree ft
																					 where ft.from_rsf_pfcbl_id = v_rsf_pfcbl_id::int)
	 and tip.to_calculate_indicator_id = v_indicator_id
)

insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date)
select 
	calcs.to_calculate_rsf_pfcbl_id,
	calcs.to_calculate_indicator_id,
	calcs.reporting_asof_date
from calculations calcs
inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = calcs.to_calculate_rsf_pfcbl_id
																													 and pis.formula_id = calcs.to_calculate_formula_id
where pis.is_subscribed = true
	and pis.is_calculated = true
	and calcs.to_calculate_rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                            from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                            where ft.from_rsf_pfcbl_id = v_rsf_pfcbl_id::int
                                              and ft.pfcbl_hierarchy <> 'parent')
	and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
						 where rpr.rsf_pfcbl_id = calcs.to_calculate_rsf_pfcbl_id
							 and rpr.reporting_asof_date = calcs.reporting_asof_date)

union all

-- if ever reported, manually or calculated--recalculate it.
select 
	rd.rsf_pfcbl_id,
	rd.indicator_id,
	rd.reporting_asof_date
from p_rsf.rsf_data_current rd
inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = rd.rsf_pfcbl_id
																													 and pis.indicator_id = rd.indicator_id
where pis.is_subscribed = true
	and pis.is_calculated = true
	and rd.indicator_id = v_indicator_id
	and rd.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                            from p_rsf.view_rsf_pfcbl_id_family_tree ft
                            where ft.from_rsf_pfcbl_id = v_rsf_pfcbl_id::int
                              and ft.pfcbl_hierarchy <> 'parent')
	and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
						 where rpr.rsf_pfcbl_id = rd.rsf_pfcbl_id
							 and rpr.reporting_asof_date = rd.reporting_asof_date)
on conflict do nothing;
*/
return true;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_program_facility_template_headers_normalized
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_program_facility_template_headers_normalized"();
CREATE FUNCTION "p_rsf"."rsf_program_facility_template_headers_normalized"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  NEW.template_header_full_normalized := coalesce(trim(concat(normalizeLabel(NEW.template_header_sheet_name),normalizeLabel(NEW.template_header))),'');
  NEW.action_mapping := coalesce(NEW.action,'default') || greatest(NEW.map_indicator_id,NEW.map_formula_id,NEW.map_check_formula_id,0);

  return NEW;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_setup_checks_auto_monitor_parameters
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_checks_auto_monitor_parameters"();
CREATE FUNCTION "p_rsf"."rsf_setup_checks_auto_monitor_parameters"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  if not exists(select * from changed)
	then 
		return NULL;
	end if;
  
  
  raise info 'rsf_setup_checks_auto_monitor_parameters(%) depth=%',
TG_OP,pg_trigger_depth();

return null;

  with changes as (
    select distinct on(ch.rsf_pfcbl_id,ch.check_formula_id)
    ch.rsf_pfcbl_id,
    ch.check_formula_id,
    ch.auto_subscribed_by_reporting_cohort_id
    from changed ch
    where ch.is_subscribed is true
    order by 
    ch.rsf_pfcbl_id,ch.check_formula_id,
    ch.auto_subscribed_by_reporting_cohort_id asc nulls last   
  )     
	insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
                                          indicator_id,
                                          formula_id,
                                          rsf_program_id,
                                          rsf_facility_id,
                                          is_subscribed,
                                          is_auto_subscribed,
                                          subscription_comments,
                                          auto_subscribed_by_reporting_cohort_id)
  select 
  x.rsf_pfcbl_id,
  x.indicator_id,
  x.formula_id,
  x.rsf_program_id,
  x.rsf_facility_id,
  x.is_subscribed,
  x.is_auto_subscribed,
  x.subscription_comments,
  x.auto_subscribed_by_reporting_cohort_id
  from (
  select 
   sis.category_manager_rsf_pfcbl_id as rsf_pfcbl_id,
   sis.indicator_id,
   sis.formula_id, -- will be the default and/or previously unsubscrivbed formula for subscription entity
   ids.rsf_program_id,
   ids.rsf_facility_id,
   true as is_subscribed,
   true as is_auto_subscribed, 
   'SYSTEM: Auto for check prerequisite check formula ' || 
   array_to_string(array_agg(distinct changes.check_formula_id),',') as subscription_comments,
   min(changes.auto_subscribed_by_reporting_cohort_id) as auto_subscribed_by_reporting_cohort_id
  from changes
  inner join p_rsf.indicator_check_formula_parameters cfp on cfp.check_formula_id = changes.check_formula_id
  inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = changes.rsf_pfcbl_id
                                                             and sis.indicator_id = cfp.parameter_indicator_id
  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = changes.rsf_pfcbl_id
  where sis.is_subscribed is false -- already subscribed means do nothing.
    and sis.category_manager_rsf_pfcbl_id is not null
  group by 
   sis.category_manager_rsf_pfcbl_id,
   sis.indicator_id,
   sis.formula_id, -- will be the default and/or previously unsubscrivbed formula for subscription entity
   ids.rsf_program_id,
   ids.rsf_facility_id
  ) x
  where not exists(select * from p_rsf.rsf_setup_indicators rsi
                   where rsi.rsf_pfcbl_id = x.rsf_pfcbl_id
                     and rsi.indicator_id = x.indicator_id
                     and rsi.is_subscribed is true)
  on conflict(rsf_pfcbl_id,indicator_id)
	do update 
  set is_subscribed = EXCLUDED.is_subscribed,
      subscription_comments = concat(rsf_setup_indicators.subscription_comments,'\n ',EXCLUDED.subscription_comments);
	return NULL;														 
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_setup_checks_subscription_allowed
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_checks_subscription_allowed"();
CREATE FUNCTION "p_rsf"."rsf_setup_checks_subscription_allowed"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
declare data_category_rank int;
declare pfcbl_category_rank int;
BEGIN
  

	select ids.pfcbl_category_rank
	into pfcbl_category_rank
	from p_rsf.rsf_pfcbl_ids ids
	where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id;
	
	select icf.check_pfcbl_rank
	into data_category_rank
	from p_rsf.indicator_check_formulas icf 
	where icf.check_formula_id = NEW.check_formula_id;


  if (pfcbl_category_rank > 2) 
  then
    raise exception 'Only Global, Program and Facility may setup check subscriptions: Global for Global Checks; Program for Program Checks; Facility for Facility+ Checks';
  end if;
  
  -- ranks above 2 will be maximized to 2
  -- so if a global (rank 0) subscribed to a loan indicator (rank 5) then 0<>2
	if (data_category_rank is not distinct from pfcbl_category_rank) 
     or
     (data_category_rank >= 2 and pfcbl_category_rank = 2)
  then
        return NEW;
  else 


      if (pfcbl_category_rank = 1) 
      then 
        raise notice 'Only Global, Program and Facility entities can set check subscriptions.  Global to global checks.  Program to program checks.  And facility to all facility+ checks.  This program "%" is auto-subscribed "%" to all available facilities. See subscription comments',
        (select sys_name from p_rsf.view_current_entity_names_and_ids where rsf_pfcbl_id = NEW.rsf_pfcbl_id),
        (select check_formula_title from p_rsf.indicator_check_formulas where check_formula_id = NEW.check_formula_id);
        
        
        insert into p_rsf.rsf_setup_checks(rsf_pfcbl_id,
                                            check_formula_id,
                                            indicator_check_id,                                                          
                                            rsf_program_id,
                                            rsf_facility_id,
                                            is_subscribed,
                                            is_auto_subscribed,
                                            subscription_comments,
                                            comments_user_id,
                                            auto_subscribed_by_reporting_cohort_id)
        select distinct
        ids.rsf_pfcbl_id,
        NEW.check_formula_id,
        NEW.indicator_check_id,
        ids.rsf_program_id,
        ids.rsf_facility_id,
        NEW.is_subscribed,
        true as is_auto_subscribed,
        concat('SYSTEM: {program > facility} subscription auto-cascade from program-level to all its facilities.','\n' || NEW.subscription_comments),
        NEW.comments_user_id,
        NEW.auto_subscribed_by_reporting_cohort_id
        from p_rsf.rsf_pfcbl_ids ids 
        where ids.rsf_program_id = NEW.rsf_pfcbl_id
          and ids.pfcbl_category = 'facility'
          and ids.rsf_pfcbl_id is distinct from NEW.rsf_pfcbl_id
          and ids.rsf_pfcbl_id is not null
          and not exists(select * from p_rsf.rsf_setup_checks pfc
                         where pfc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                           and pfc.check_formula_id = NEW.check_formula_id);
        
        return NULL;
        
      end if;

  
      raise exception 'Only Global, Program and Facility entities can set check subscriptions.  Global to global checks.  Program to program checks.  And facility to all facility+ checks.  Failed subscription for % and check formula=% because pfcbl_category_rank=% and data_category_rank %',
      (select sys_name from p_rsf.view_current_entity_names_and_ids where rsf_pfcbl_id = NEW.rsf_pfcbl_id),
      NEW.check_formula_id,pfcbl_category_rank,data_category_rank;
        return NULL;
  end if;
  
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_setup_indicators_auto_monitor_checks
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_indicators_auto_monitor_checks"();
CREATE FUNCTION "p_rsf"."rsf_setup_indicators_auto_monitor_checks"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  if not exists(select * from changed)
	then 
		return NULL;
	end if;
	
  
  
  raise info 'rsf_setup_indicators_auto_monitor_checks(%) depth=%',
TG_OP,pg_trigger_depth();

  if TG_OP <> 'DELETE' then

   with changes as (
    select distinct on(ch.rsf_pfcbl_id)
    ch.rsf_pfcbl_id,
    ch.auto_subscribed_by_reporting_cohort_id
    from changed ch
    order by 
    ch.rsf_pfcbl_id,
    ch.auto_subscribed_by_reporting_cohort_id asc nulls last   
   )
   insert into p_rsf.rsf_setup_checks(rsf_pfcbl_id,
                                      check_formula_id,
                                      indicator_check_id,
                                      rsf_program_id,
                                      rsf_facility_id,
                                      is_subscribed,
                                      is_auto_subscribed,
                                      subscription_comments,
                                      auto_subscribed_by_reporting_cohort_id)	
      select distinct
        changes.rsf_pfcbl_id,
        scm.check_formula_id,
        scm.indicator_check_id,
        ids.rsf_program_id,
        ids.rsf_facility_id,
        true as is_subscribed,
        true as is_auto_subscribed,
        'SYSTEM: Auto-subscribed to check, triggered by monitoring all required parameters',
        changes.auto_subscribed_by_reporting_cohort_id
      from changes
      inner join p_rsf.view_rsf_setup_check_monitoring scm on scm.rsf_pfcbl_id = changes.rsf_pfcbl_id
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = scm.rsf_pfcbl_id     
      where scm.is_auto_monitorable is true
        and scm.filter_category_manager is true -- will ensure that changes.rsf_pfcbl_id only inserts global=global, etc.
        
      on conflict(rsf_pfcbl_id,check_formula_id)      
      do nothing; -- in case its there and set deliberately to false	

    end if;
      
    update p_rsf.rsf_setup_checks pfc
    set is_subscribed = false,
        is_auto_subscribed = true,
        subscription_comments = concat(pfc.subscription_comments,' \nSYSTEM: Auto-unsubscribed to check, triggered by setting to not monitor a required input parameter')
    from p_rsf.view_rsf_setup_check_monitoring scm
    where scm.rsf_pfcbl_id = any (select distinct changed.rsf_pfcbl_id from changed)
      and scm.is_not_monitorable is true -- has an unsubscribed parameter
      and pfc.is_auto_subscribed is true
      and pfc.is_subscribed is true
      and pfc.rsf_pfcbl_id = scm.rsf_pfcbl_id
      and pfc.check_formula_id = scm.check_formula_id;


	return NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_setup_indicators_auto_monitor_parameters
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_indicators_auto_monitor_parameters"();
CREATE FUNCTION "p_rsf"."rsf_setup_indicators_auto_monitor_parameters"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

 
  if not exists(select * from changed)
	then 
		return NULL;
	end if;
 
raise info 'rsf_setup_indicators_auto_monitor_parameters(%) depth=%',
TG_OP,pg_trigger_depth();

     with changes as (
      select distinct on(ch.rsf_pfcbl_id,ch.indicator_id)
      ch.rsf_pfcbl_id,
      ch.indicator_id,
      ch.formula_id,
      ch.auto_subscribed_by_reporting_cohort_id
      from changed ch
      where ch.is_subscribed is true
      order by 
      ch.rsf_pfcbl_id,ch.indicator_id,
      ch.formula_id asc nulls last,
      ch.auto_subscribed_by_reporting_cohort_id asc nulls last   
     )
     insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
                                            indicator_id,
                                            formula_id,
                                            rsf_program_id,
                                            rsf_facility_id,
                                            is_subscribed,
                                            is_auto_subscribed,
                                            subscription_comments,
                                            auto_subscribed_by_reporting_cohort_id)
      select
        sis.category_manager_rsf_pfcbl_id as rsf_pfcbl_id,
        sis.indicator_id,
        sis.formula_id,
        ids.rsf_program_id,
        ids.rsf_facility_id,
        true as is_subscribed,
        true as is_auto_subscribed,
        'SYSTEM: Auto indicator prerequisite from calculation formula ' || changed.formula_id || 
          '/' || (select indicator_name from p_rsf.indicators where indicators.indicator_id = ifp.parameter_indicator_id) as subscription_comments,
        changed.auto_subscribed_by_reporting_cohort_id
      from changed
      inner join p_rsf.indicator_formula_parameters ifp on ifp.formula_id = changed.formula_id
      inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = changed.rsf_pfcbl_id
                                                                 and sis.indicator_id = ifp.parameter_indicator_id
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = sis.rsf_pfcbl_id
      where sis.category_manager_rsf_pfcbl_id is not null
      on conflict(rsf_pfcbl_id,indicator_id)
      do nothing;
          
  
      
	return NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_setup_indicators_delete_calculated_data
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_indicators_delete_calculated_data"();
CREATE FUNCTION "p_rsf"."rsf_setup_indicators_delete_calculated_data"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN
    
	if (TG_OP = 'DELETE') then
  
    with remove as (
      select 
        del.rsf_pfcbl_id,
        del.indicator_id
      from deleted del
      where del.is_subscribed is true
        and del.formula_id is not null
    ),
    calcs as (
      select
        ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id,
        ind.indicator_id
      from remove rem  
      inner join p_rsf.indicators ind on ind.indicator_id = rem.indicator_id
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = rem.rsf_pfcbl_id
                                                         and ft.to_pfcbl_category = ind.data_category
      where ft.pfcbl_hierarchy <> 'parent'		
    )     
    delete from p_rsf.rsf_data_calculation_evaluations dce
    using calcs
    where calcs.rsf_pfcbl_id = dce.rsf_pfcbl_id
      and calcs.indicator_id = dce.indicator_id; 
      
    with remove as (
      select 
        del.rsf_pfcbl_id,
        del.indicator_id
      from deleted del
      where del.is_subscribed is true
        and del.formula_id is not null
    ),
    calcs as (
      select 
        rd.data_id
      from remove rem
      inner join p_rsf.indicators ind on ind.indicator_id = rem.indicator_id
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = rem.rsf_pfcbl_id
                                                       and ft.to_pfcbl_category = ind.data_category
      inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                  and rd.indicator_id = rem.indicator_id
      inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
      where rc.is_calculated_cohort is true       
        and ft.pfcbl_hierarchy <> 'parent'		                                                                                  
    )
    delete from p_rsf.rsf_data rd
    using calcs 
    where calcs.data_id = rd.data_id;
    
	elseif (TG_OP = 'UPDATE') then
  
  
    with remove as (
      select 
        ch.rsf_pfcbl_id,
        ch.indicator_id,
        ch.is_subscribed,
        ch.formula_id
      from changed ch
        
      except
      
      select 
        del.rsf_pfcbl_id,
        del.indicator_id,
        del.is_subscribed,
        del.formula_id
      from deleted del
    ),
    calcs as (
      select
        ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id,
        ind.indicator_id
      from remove rem  
      inner join p_rsf.indicators ind on ind.indicator_id = rem.indicator_id
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = rem.rsf_pfcbl_id
                                                       and ft.to_pfcbl_category = ind.data_category
      where ft.pfcbl_hierarchy <> 'parent'		
        and (rem.formula_id is NULL or rem.is_subscribed is false)     
    )     
    delete from p_rsf.rsf_data_calculation_evaluations dce
    using calcs
    where calcs.rsf_pfcbl_id = dce.rsf_pfcbl_id
      and calcs.indicator_id = dce.indicator_id; 
      
    with remove as (
        select 
          ch.rsf_pfcbl_id,
          ch.indicator_id,
          ch.is_subscribed,
          ch.formula_id
        from changed ch
          
        except
        
        select 
          del.rsf_pfcbl_id,
          del.indicator_id,
          del.is_subscribed,
          del.formula_id
        from deleted del
      ),
      calcs as (
        select 
          rd.data_id
        from remove rem
        inner join p_rsf.indicators ind on ind.indicator_id = rem.indicator_id
        inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = rem.rsf_pfcbl_id
                                                         and ft.to_pfcbl_category = ind.data_category
        inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                    and rd.indicator_id = rem.indicator_id
        inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
        where rc.is_calculated_cohort is true 
          and ft.pfcbl_hierarchy <> 'parent'		
          and (rem.formula_id is NULL or rem.is_subscribed is false)                                                                               
      )
      delete from p_rsf.rsf_data rd
      using calcs 
      where calcs.data_id = rd.data_id;
  
	end if;
		
	return NULL;
	
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_setup_indicators_subscription_allowed
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_indicators_subscription_allowed"();
CREATE FUNCTION "p_rsf"."rsf_setup_indicators_subscription_allowed"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
declare data_category_rank int;
declare pfcbl_category_rank int;
BEGIN
  
/*
  raise info 'rsf_setup_indicators_subscription_allowed(%):depth=%: is indicator_id=% allowed for rsf_pfcbl_id=%',
  TG_OP,pg_trigger_depth(),NEW.indicator_id,NEW.rsf_pfcbl_id;
*/
	select ids.pfcbl_category_rank
	into pfcbl_category_rank
	from p_rsf.rsf_pfcbl_ids ids
	where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id;
	
	select ind.pfcbl_rank
	into data_category_rank
	from p_rsf.indicators ind 
	where ind.indicator_id = NEW.indicator_id;

  if (pfcbl_category_rank > 2) 
  then
    raise exception 'Only Global, Program and Facility may setup indicator subscriptions: Global for Global Indicators; Program for Program Indicators; Facility for Facility+ Indicators';
  end if;
  
  -- ranks above 2 will be maximized to 2
  -- so if a global (rank 0) subscribed to a loan indicator (rank 5) then 0<>2
	if (data_category_rank is not distinct from pfcbl_category_rank) 
     or
     (data_category_rank >= 2 and pfcbl_category_rank = 2)
  then
        return NEW;
  else 


      if (pfcbl_category_rank = 1) 
      then 
        raise notice 'Only Global, Program and Facility entities can set indicator subscriptions.  Global to global indicators.  Program to program indicators.  And facility to all facility+ indicators.  This program "%" is auto-subscribed "%" to all available facilities. See subscription comments',
        (select sys_name from p_rsf.view_current_entity_names_and_ids where rsf_pfcbl_id = NEW.rsf_pfcbl_id),
        (select indicator_name from p_rsf.indicators where indicator_id = NEW.indicator_id);
        
        insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
                                                          indicator_id,
                                                          formula_id,
                                                          rsf_program_id,
                                                          rsf_facility_id,
                                                          is_subscribed,
                                                          is_auto_subscribed,
                                                          subscription_comments,
                                                          comments_user_id,
                                                          options_group_id,
                                                          formula_calculation_unit,
                                                          auto_subscribed_by_reporting_cohort_id)
        select distinct
        ids.rsf_pfcbl_id,
        NEW.indicator_id,
        NEW.formula_id,
        ids.rsf_program_id,
        ids.rsf_facility_id,
        NEW.is_subscribed,
        true as is_auto_subscribed,
        concat('SYSTEM: {program > facility} subscription auto-cascade from program-level to all its facilities.','\n' || NEW.subscription_comments),
        NEW.comments_user_id,
        NEW.options_group_id,
        NEW.formula_calculation_unit,
        NEW.auto_subscribed_by_reporting_cohort_id
        from p_rsf.rsf_pfcbl_ids ids 
        where ids.rsf_program_id = NEW.rsf_pfcbl_id
          and ids.pfcbl_category = 'facility'
          and ids.rsf_pfcbl_id is distinct from NEW.rsf_pfcbl_id
          and ids.rsf_pfcbl_id is not null
          and not exists(select * from p_rsf.rsf_setup_indicators pfi
                         where pfi.rsf_pfcbl_id = ids.rsf_pfcbl_id
                           and pfi.indicator_id = NEW.indicator_id);
        
        return NULL;
        
      end if;

  
      raise exception 'Only Global, Program and Facility entities can set indicator subscriptions.  Global to global indicators.  Program to program indicators.  And facility to all facility+ indicators.  Failed subscription for % and indicator=% because pfcbl_category_rank=% and data_category_rank %',
      (select sys_name from p_rsf.view_current_entity_names_and_ids where rsf_pfcbl_id = NEW.rsf_pfcbl_id),NEW.indicator_id,pfcbl_category_rank,data_category_rank;
        return NULL;
  end if;
  
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_setup_indicators_subscription_recalculations
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_indicators_subscription_recalculations"();
CREATE FUNCTION "p_rsf"."rsf_setup_indicators_subscription_recalculations"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  
  if not exists(select * from changed)
	then 
		return NULL;
	end if;
                  
raise notice 'rsf_setup_indicators_subscription_recalculations(%) and trigger_depth=%',
  TG_OP,
	pg_trigger_depth();
	
  
  if TG_OP = 'INSERT' then
    perform * from (
      select 
      p_rsf.rsf_pfcbl_indicator_recalculate(v_rsf_pfcbl_id => ch.rsf_pfcbl_id,
                                            v_formula_id => ch.formula_id) as recalc
      from (
        select distinct
          changed.rsf_pfcbl_id,
          changed.formula_id
        from changed
        where changed.formula_id is not null
          and changed.is_subscribed is true
      ) as ch
    );
  
  elseif TG_OP = 'UPDATE' then

      perform * from (
      select 
        p_rsf.rsf_pfcbl_indicator_recalculate(v_rsf_pfcbl_id => ch.rsf_pfcbl_id,
                                              v_formula_id => ch.formula_id) as calc
      from (
       select distinct
        changed.rsf_pfcbl_id,
        changed.formula_id,
        changed.formula_calculation_unit
        from changed
        where changed.formula_id is not null
          and changed.is_subscribed is true
        
        except
        
        select 
        previous.rsf_pfcbl_id,
        previous.formula_id,
        previous.formula_calculation_unit
        from previous     
     ) as ch);

  end if;

	return NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_setup_indicators_validate_calculation_unit
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_indicators_validate_calculation_unit"();
CREATE FUNCTION "p_rsf"."rsf_setup_indicators_validate_calculation_unit"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  if (NEW.formula_calculation_unit is NULL) 
  then
    return NEW; -- always okay to have no custom calculation unit
  else
  
    if NEW.formula_id is NULL
    then
      -- if there is no formula, then there cannot be an overwrite. 
      raise info 'Removing calculation formula unit % because formula_id has been removed for %',
      (NEW.formula_calculation_unit),
      (select ind.indicator_name from p_rsf.indicators ind where ind.indicator_id = NEW.indicator_id);
      
      NEW.formula_calculation_unit := NULL;
      
      return NEW;
    elseif exists(select * from p_rsf.indicator_formulas indf
                  where indf.formula_id = NEW.formula_id
                    and indf.formula_fx_date = 'nofx')
    then 
      raise exception 'Calculation formula unit cannot be set because Calculation FX Date is set to "Parameter Currency (NO FX)"';                            
                            
    elseif exists(select * from p_rsf.indicator_formulas indf
                  inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
                  where indf.formula_id = NEW.formula_id
                    and ind.data_type = 'currency'
                    and ind.data_unit = 'LCU')
    then
      -- okay: I have an LCU currency
      return NEW;
      
    elseif exists(select * from p_rsf.indicator_formulas indf
                  inner join p_rsf.indicators ind on ind.indicator_id = any(indf.formula_indicator_ids)
                   where indf.formula_id = NEW.formula_id
                     and ind.data_type = 'currency')
    then                     
      -- okay: formula has a currency as its input and so we can enforce those units are converted before calculation.
      return NEW;
    else 
      raise exception 'Calculation formula unit (1) Must be a valid 3-letter currency, eg USA, EUR, JPY (2) Can only be set for LCU currency indicators; or (3) Formulas with currency parameters';
    
    end if; 
    
    return NEW;
  end if;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for set_calculation_formula_parameters
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_calculation_formula_parameters"();
CREATE FUNCTION "p_rsf"."set_calculation_formula_parameters"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE subscriptions record;
BEGIN

  	delete from p_rsf.indicator_formula_parameters ifp
		where ifp.formula_id = NEW.formula_id;
			
		insert into p_rsf.indicator_formula_parameters(formula_id,
																									 indicator_id,
																									 calculate_pfcbl_category,
																									 calculate_grouping_pfcbl_category,
																									 calculate_grouping_pfcbl_rank,
																									 parameter_indicator_id,
																									 parameter_pfcbl_category,
																									 parameter_pfcbl_rank,
																									 parameter_pfcbl_hierarchy,
																									 parameter_is_current,
																									 parameter_is_previous,
																									 parameter_is_info,
																									 parameter_is_all,
																									 parameter_trigger_by_reporting,
																									 parameter_data_type)
	  select
			NEW.formula_id,
			NEW.indicator_id,
			ind.data_category,
			rpc_g.pfcbl_category,
			rpc_g.pfcbl_rank,
			pid.parameter_indicator_id,
			rpc_p.pfcbl_category,
			rpc_p.pfcbl_rank,
			case when rpc_p.pfcbl_rank < rpc_f.pfcbl_rank then 'parent'
			     when rpc_p.pfcbl_rank = rpc_f.pfcbl_rank then 'self'
					 when rpc_p.pfcbl_rank > rpc_f.pfcbl_rank then 'child'
					 else NULL end as parameter_pfcbl_hierarchy,

		  coalesce(NEW.formula ~ (ind_p.indicator_name || '\.current'),false) as parameter_is_current,
			coalesce(NEW.formula ~ (ind_p.indicator_name || '\.previous'),false) as parameter_is_previous,
			coalesce(NEW.formula ~ (ind_p.indicator_name || '\.info'),false) as parameter_is_info,
			coalesce(NEW.formula ~ (ind_p.indicator_name || '\.all'),false) as parameter_is_all,
			
			-- entity_reporting triggered by reporting
			-- but also calculation that must look at .previous since if this specific indicator didn't report, the current value
			-- relative to its previous could change.
			coalesce(ind_p.indicator_sys_category = 'entity_reporting',false) OR
      coalesce(NEW.formula ~ (ind_p.indicator_name || '\.all'),false) OR
			coalesce(NEW.formula ~ (ind_p.indicator_name || '\.previous'),false) as parameter_trigger_by_reporting,
			ind_p.data_type as parameter_data_type

		from p_rsf.indicators ind
		inner join p_rsf.rsf_pfcbl_categories rpc_f on rpc_f.pfcbl_category = ind.data_category
		inner join lateral (select distinct unnest(NEW.formula_indicator_ids) as parameter_indicator_id) pid on true -- should alrady be distinct
		inner join p_rsf.indicators ind_p on ind_p.indicator_id = pid.parameter_indicator_id
		inner join p_rsf.rsf_pfcbl_categories rpc_p on rpc_p.pfcbl_category = ind_p.data_category
		left join p_rsf.rsf_pfcbl_categories rpc_g on rpc_g.pfcbl_rank = NEW.formula_grouping_pfcbl_rank
		                                          and rpc_g.pfcbl_rank < rpc_f.pfcbl_rank -- it's only meaningful to group at a parent level
		where ind.indicator_id = NEW.indicator_id;
		 
    ------------------------------------------------------------------------------------------------
		refresh materialized view p_rsf.compute_calculation_to_parameter_categories;
		------------------------------------------------------------------------------------------------
    
   
  perform (
    select pfi.rsf_pfcbl_id,pfi.formula_id,recalc
    from p_rsf.rsf_setup_indicators pfi
    inner join lateral p_rsf.rsf_pfcbl_indicator_recalculate(v_rsf_pfcbl_id => pfi.rsf_pfcbl_id,
                                                             v_formula_id => pfi.formula_id) as recalc on true
    where pfi.indicator_id = NEW.indicator_id
      and pfi.formula_id is not null
      and pfi.is_subscribed is true);

	RETURN NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for set_exporting_cohorts_reporting_key
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_exporting_cohorts_reporting_key"();
CREATE FUNCTION "p_rsf"."set_exporting_cohorts_reporting_key"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN
  NEW.exporting_time := NOW();
	NEW.reporting_key := lower(md5(concat(NEW.exporting_rsf_pfcbl_id::text,
																				NEW.exporting_asof_date::text,
																				NEW.data_integrity_key,
																				NEW.exporting_time::text)));
	RETURN NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for set_indicator_check_formula_parameters
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_indicator_check_formula_parameters"();
CREATE FUNCTION "p_rsf"."set_indicator_check_formula_parameters"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  	delete from p_rsf.indicator_check_formula_parameters cfp
		where cfp.check_formula_id = NEW.check_formula_id;
			
		insert into p_rsf.indicator_check_formula_parameters(indicator_check_id,
																												 check_formula_id,
																												 for_pfcbl_category,
																												 check_grouping_pfcbl_rank,
																												 check_grouping_pfcbl_category,
																												 parameter_indicator_id,
																												 parameter_pfcbl_category,
																												 parameter_pfcbl_rank,
																												 parameter_pfcbl_hierarchy,
																												 is_calculation_trigger_parameter,
																												 parameter_trigger_by_reporting)
	  select
			NEW.indicator_check_id,
			NEW.check_formula_id,
			NEW.check_pfcbl_category as for_pfcbl_category,
			NEW.parent_grouping_pfcbl_rank,
			NEW.parent_grouping_pfcbl_category,
			pid.parameter_indicator_id,
			rpc.pfcbl_category,
			rpc.pfcbl_rank,
  		case when rpc.pfcbl_rank < NEW.check_pfcbl_rank then 'parent'
			     when rpc.pfcbl_rank = NEW.check_pfcbl_rank then 'self'
					 when rpc.pfcbl_rank > NEW.check_pfcbl_rank then 'child'
					 else NULL end as parameter_pfcbl_hierarchy,
		  pid.parameter_indicator_id = any(NEW.check_formula_indicator_ids) as is_calculation_trigger_parameter,
			ind.indicator_sys_category is not distinct from 'entity_reporting' as parameter_trigger_by_reporting
		from (select unnest(NEW.formula_indicator_ids) as parameter_indicator_id) pid
		inner join p_rsf.indicators ind on ind.indicator_id = pid.parameter_indicator_id
		inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
		left join p_rsf.rsf_pfcbl_categories g_rpc on g_rpc.pfcbl_category = NEW.parent_grouping_pfcbl_category;
		
		/* No because aggregate level checks may not use parent-level parameters -- in this case, default to flag the check pfcbl_category reporting
		if (not exists(select * from p_rsf.indicator_check_formula_parameters icfp
		               where icfp.check_formula_id = NEW.check_formula_id
									   and icfp.parameter_pfcbl_category = NEW.check_pfcbl_category
										 and icfp.is_calculation_trigger_parameter = true))
    then
			raise exception 'indicator_check_id defined at %-level and must have at least one calculation parameter at %-level',
			(NEW.check_pfcbl_category),(NEW.check_pfcbl_category);
		end if;										 
		*/
		------------------------------------------------------------------------------------------------
		refresh materialized view CONCURRENTLY p_rsf.compute_check_to_parameter_categories;
		------------------------------------------------------------------------------------------------
		
		-- groups will be by FORMULA_INDICATOR_IDS although the actual query may query messaging indicators from another category
		update p_rsf.indicator_check_formulas icf
		set computation_group = ccg.computation_group
		from p_rsf.compute_check_grouping ccg
		where icf.check_formula_id = ccg.check_formula_id
			and icf.check_formula_id = NEW.check_formula_id;	
      
      
    /*****/
    with reporting as (
      select 
        ids.rsf_pfcbl_id,
        max(rc.reporting_cohort_id) as auto_subscribed_by_reporting_cohort_id
      from p_rsf.rsf_pfcbl_ids ids
      inner join p_rsf.reporting_cohorts rc on rc.reporting_rsf_pfcbl_id = ids.rsf_pfcbl_id
      where rc.is_reported_cohort is true
        and ids.pfcbl_category_rank = (select least(2,icf.check_pfcbl_rank)
                                       from p_rsf.indicator_check_formulas icf
                                       where icf.check_formula_id = NEW.check_formula_id)
      group by ids.rsf_pfcbl_id
   )
   insert into p_rsf.rsf_setup_checks(rsf_pfcbl_id,
                                      check_formula_id,
                                      indicator_check_id,
                                      rsf_program_id,
                                      rsf_facility_id,
                                      is_subscribed,
                                      is_auto_subscribed,
                                      subscription_comments,
                                      auto_subscribed_by_reporting_cohort_id)	
      select distinct
        reporting.rsf_pfcbl_id,
        scm.check_formula_id,
        scm.indicator_check_id,
        ids.rsf_program_id,
        ids.rsf_facility_id,
        true as is_subscribed,
        true as is_auto_subscribed,
        'SYSTEM: Auto-subscribed to check, triggered by monitoring all required parameters',
        reporting.auto_subscribed_by_reporting_cohort_id
      from reporting
      inner join p_rsf.view_rsf_setup_check_monitoring scm on scm.rsf_pfcbl_id = reporting.rsf_pfcbl_id
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = scm.rsf_pfcbl_id     
      where scm.check_formula_id = NEW.check_formula_id
        and scm.is_auto_monitorable is true
        and scm.filter_category_manager is true -- will ensure that changes.rsf_pfcbl_id only inserts global=global, etc.
        
      on conflict(rsf_pfcbl_id,check_formula_id)      
      do nothing; -- in case its there and set deliberately to false	
     
    update p_rsf.rsf_setup_checks pfc
    set is_subscribed = false,
        is_auto_subscribed = true,
        subscription_comments = concat(pfc.subscription_comments,' \nSYSTEM: Auto-unsubscribed to check, triggered by setting to not monitor a required input parameter')
    from p_rsf.view_rsf_setup_check_monitoring scm
    where scm.check_formula_id = NEW.check_formula_id
      and scm.is_not_monitorable is true -- has an unsubscribed parameter
      and pfc.is_auto_subscribed is true
      and pfc.is_subscribed is true
      and pfc.rsf_pfcbl_id = scm.rsf_pfcbl_id
      and pfc.check_formula_id = scm.check_formula_id;
    
    /*****/
    
    
    
		
    perform * from (
		 select pfc.rsf_pfcbl_id,pfc.check_formula_id,recalc
      from p_rsf.rsf_setup_checks pfc
      inner join lateral p_rsf.rsf_pfcbl_check_recalculate(v_rsf_pfcbl_id => pfc.rsf_pfcbl_id,
                                                           v_check_formula_id => pfc.check_formula_id) as recalc on true
      where pfc.check_formula_id = NEW.check_formula_id
        and pfc.is_subscribed is true);
       
			-- not filtering on is calculation trigger because we want new messages if messages have changed
		
	RETURN NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for set_indicator_check_ids
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_indicator_check_ids"();
CREATE FUNCTION "p_rsf"."set_indicator_check_ids"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

    -- resetting to blank in case user submitted data 
    new.formula_indicator_ids := array[]::int[];
		new.check_formula_indicator_ids := array[]::int[];
		new.check_message_indicator_ids := array[]::int[];
		
		new.check_formula_indicator_ids := (select array_agg(distinct ind.indicator_id)::int[]
		                                   from p_rsf.indicators ind
																			 where new.formula::text like '%' || ind.indicator_name || '.%');

		new.check_message_indicator_ids := (select array_agg(distinct ind.indicator_id)::int[]
		                                   from p_rsf.indicators ind
																			 where new.formula_result_message::text like '%' || ind.indicator_name || '.%');
																			 
	  select 
			rpc.pfcbl_rank
		into 
			NEW.check_pfcbl_rank
		from p_rsf.rsf_pfcbl_categories rpc		
		where rpc.pfcbl_category = NEW.check_pfcbl_category;
		
		if NEW.check_pfcbl_rank is NULL
		then
			raise exception 'Failed to resovle pfcbl_rank from reported check_pfcbl_category=%',
			NEW.check_pfcbl_category;
		end if;
		--If formula has no parameters at the level for which the check is assigned, then add that level's entity_reporting indicator
		--to ensure that the system can apply it correctly, as we should apply checks on the lastest-reported variable input that triggered
		--the check to calculate.  Where there are no parameters at the check's level, then add-in reporting so that it will (re)check when
		--that entity reports.  Otherwise, the entity could re-report its entity_reporting data without any updates of the parameters that compute
		--the check and result in it being overwritten and losing the flag.
		if (not exists(select * 
		               from p_rsf.indicators ind
									 where ind.indicator_id = any(new.check_formula_indicator_ids)
									   and ind.data_category = new.check_pfcbl_category))
    then 
			 select new.check_formula_indicator_ids || ind.indicator_id 
			 into new.check_formula_indicator_ids
			 from p_rsf.indicators ind 
			 where ind.indicator_sys_category = 'entity_reporting'
				 and ind.data_category = new.check_pfcbl_category;
		end if;
			
		if exists(select * 
						  from p_rsf.indicator_checks ic
						  inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ic.grouping
						  where ic.indicator_check_id = NEW.indicator_check_id
						    and rpc.pfcbl_rank > NEW.check_pfcbl_rank)
		then
			raise exception 'Check % is grouped at %, therefore its formula ID % cannot be applied at parent-level %',
			(select ic.check_name from p_rsf.indicator_checks ic where ic.indicator_check_id = NEW.indicator_check_id),
			(select ic.grouping from p_rsf.indicator_checks ic where ic.indicator_check_id = NEW.indicator_check_id),
			(NEW.formula_check_id),
			(NEW.check_pfcbl_category);
		
		end if;
						 
	  -- check-level subgrouping is an indicator's "current" value.  So if check defines a subgrouping, include that indicator_id as well, as its
		-- necessary to calculate the check
		-- subgrouping would take form of, eg, 'borrower_age_at_commitment.current'
			NEW.check_formula_indicator_ids := NEW.check_formula_indicator_ids || 
																				 coalesce(
																					(
																					 select array_agg(ind.indicator_id) 
																					 from p_rsf.indicators ind
																					 inner join p_rsf.indicator_checks ic on ic.subgrouping like '%' || ind.indicator_name || '.%'
																					 where ic.indicator_check_id = new.indicator_check_id
																					),
																					array[]::int[]);
--		end if;
		NEW.formula_version_number := NEW.formula_version_number + 1;
		NEW.formula_modification_time := now();
		
		
--		NEW.formula_indicator_ids := array_remove(NEW.formula_indicator_ids,NULL);
--    NEW.formula_indicator_ids := uniq(sort(coalesce(NEW.formula_indicator_ids,array[]::int[]))); -- because sys_ checks have no formula defined 

		NEW.check_formula_indicator_ids := array_remove(NEW.check_formula_indicator_ids,NULL);
    NEW.check_formula_indicator_ids := uniq(sort(coalesce(NEW.check_formula_indicator_ids,array[]::int[]))); 
		-- because sys_ checks have no formula defined 

		NEW.check_message_indicator_ids := array_remove(NEW.check_message_indicator_ids,NULL);
    NEW.check_message_indicator_ids := uniq(sort(coalesce(NEW.check_message_indicator_ids,array[]::int[]))); 
		-- because sys_ checks have no formula defined 
		
		--Presumably a user could define a check that groups at a child pfcbl category.
		--But when would that make any sense?  Even if so, the family tree would cascade up just as if it were not grouped.
		NEW.parent_grouping_pfcbl_category :=	(select rpc.pfcbl_category
																					 from p_rsf.rsf_pfcbl_categories rpc
																					 inner join p_rsf.indicator_checks ic on ic.indicator_check_id = NEW.indicator_check_id
																					 where rpc.pfcbl_category = ic.grouping 
																					   and rpc.pfcbl_rank < NEW.check_pfcbl_rank);
																						
		NEW.parent_grouping_pfcbl_rank := (select rpc.pfcbl_rank 
																			 from p_rsf.rsf_pfcbl_categories rpc
																			 where rpc.pfcbl_category = NEW.parent_grouping_pfcbl_category);
																			 
				-- updated 2023-02-24 to selected on check_formula_indicator_ids instead of on formula_indicator_ids
		-- since indicator_ids used in messaging shouldn't trigger stale check parameters
		select sort(array_agg(distinct rpc.pfcbl_rank))
		into NEW.parameter_pfcbl_ranks
		from p_rsf.indicators ind
		inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
		where ind.indicator_id = any(NEW.check_formula_indicator_ids);


		if (cardinality(NEW.check_formula_indicator_ids)=0) then 
			NEW.check_formula_indicator_ids := NULL::int[]; -- will throw an error as check cannot be without parameter IDs
	  end if;	
		
		-- in case the message has no parameters or is static sets to an empty array to allow consistent manipulations and queries 
		if (cardinality(NEW.check_message_indicator_ids)=0) then 
		  NEW.check_message_indicator_ids := array[]::int[];
		end if;
		
		NEW.formula_indicator_ids := uniq(sort( (NEW.check_formula_indicator_ids || NEW.check_message_indicator_ids)));

	RETURN NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for set_indicator_formula_id_ranks
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_indicator_formula_id_ranks"();
CREATE FUNCTION "p_rsf"."set_indicator_formula_id_ranks"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  -- otherwise, an infinite loop of it updating itself will occur
  if (not exists(select * from changed))
  then
    raise notice 'set_indicator_formula_id_ranks: nothing changed';      
    return NULL;
  end if;
  
  raise notice 'Updating formula ranks %',pg_trigger_depth();      

	
		
    -- would cause recursive updates, except this trigger only fires on specific columns other than computation_group
		update p_rsf.indicator_formulas indf
		set computation_group = cg.computation_group::text
		from p_rsf.compute_calculation_grouping cg
		where indf.indicator_id = cg.calculate_indicator_id
		  and indf.formula_id = cg.calculate_formula_id
		  and indf.computation_group::text is distinct from cg.computation_group::text;

                                                     
  update p_rsf.indicator_formulas indf
  set formula_calculation_rank = ccr.calculation_rank,
      formula_indicator_id_requirements = ccr.formula_indicator_id_requirements,
      computation_priority_rank = case when ccr.data_type = 'currency_ratio' then 1 else 0 end
  from p_rsf.compute_calculation_ranks ccr
  where ccr.formula_id = indf.formula_id
    and (ccr.calculation_rank is distinct from indf.formula_calculation_rank
         or
         ccr.formula_indicator_id_requirements is distinct from indf.formula_indicator_id_requirements
         or
         indf.computation_priority_rank is distinct from case when ccr.data_type = 'currency_ratio' then 1 else 0 end);
		
 
 
      --NEW.formula_indicator_id_requirements := uniq(sort(NEW.formula_indicator_id_requirements));
			--raise notice '    Set formula_calculation_rank for indicator_id=% as % and requirements as %',
			--NEW.indicator_id,NEW.formula_calculation_rank,array_to_string(NEW.formula_indicator_id_requirements,',');

      if exists(select * from p_rsf.indicator_formulas indf
                inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
                where ind.data_type = 'currency_ratio'
                  and ind.data_category <> 'global'
                  and indf.formula_calculation_rank <> 1)
      then
				raise exception 'Failed to update formula ranks, triggered by indicator_id=% for: %
				                 because currency_ratio indicators must resolve a formula_calculation_rank=1.
												 Currency ratio calculations are expected to set this formula:
												 get_IFC_FX_rate(exchange_rate_date=global_reporting_quarter_end_date.current.reporteddate,
												                 currency_code_ratio={indicator_name}.current.unit)',
												 NEW.indicator_id,
                         (select array_to_string(array_agg(distinct concat(ind.indicator_name,':',indf.formula_title),', ')) 
                          from p_rsf.indicator_formulas indf
                          inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
                          where ind.data_type = 'currency_ratio'
                            and ind.data_category <> 'global'
                            and indf.formula_calculation_rank <> 1);
			end if;		  
		
RETURN NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for set_indicator_formula_ids
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_indicator_formula_ids"();
CREATE FUNCTION "p_rsf"."set_indicator_formula_ids"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE v_formula_sort_indicator_ids int[] default NULL;
DECLARE v_formula_grouping_rank int default NULL;
DECLARE v_formula_own_rank int default NULL;
/*
DECLARE counter int := 0;
DECLARE recursive_rank int := 0;
DECLARE next_rank_valid_indicator_ids int[] := array[]::int[];
DECLARE recursive_formula_indicator_ids int[] := array[]::int[];
DECLARE incremental_formula_indicator_ids int[] := array[]::int[];
*/
BEGIN
		
			raise notice 'Updating indicator_id=%', NEW.indicator_id;		
			
			--Fromula is a "timeseries" formula when it must look-up values outside the "current" timeline.
			
			if (exists(select * from p_rsf.indicators ind 
								 where ind.indicator_id = NEW.indicator_id                     -- self referential to my own indicator_id
 								   and NEW.formula ~ (ind.indicator_name || '\.') = true       -- and my own name is in the formula
									 and NEW.formula ~ (ind.indicator_name || '\.current\.unit') = false -- and its not my own current.unit
									 and NEW.formula ~ (ind.indicator_name || '\.(previous|sum\.previous|min\.previous|max\.previous)') = false))    
									 -- or a previous value
									 -- .previous re-allowed this self references for cumulative calculations; this was previously denied, but 
									 -- TRIGGER p_rsf.rsf_data_modified_calculations() will insert calculation evaluations for any future reporting
									 -- in case a "previous" value is updated; and if any current.unit values are updated, then self-reporeted 
									 -- calculations will trigger their own update
									 
		  then
				raise exception 'Calculation formulas cannot be self referential, except for: .current.unit and .previous';
			end if;
			
/* 2023-08-04: reverted to deny self referentiality.  Function was not used.  If we really need a sumsum can be done using .all	
               also updated to explicitly allow self-refentiality to own indicator unit, as this is separate than self-referencing and 
						   calculation of own value			
							-- own ID can enter in when referencing own current.unit, currently only used for FX.					
*/

			-- to ensure its reset
			NEW.formula_indicator_ids := NULL; 

/* Feb 2024 updates: rewrite of facility level calculations and deprecating formula_calculated_by_indicator_id to have multiple formula_ids			
			if NEW.formula_calculated_by_indicator_id is NULL 
			then 
*/			
				with formula_matches as (
					select 
						new.formula_id,
						new.indicator_id,						
						coalesce(new.formula,'') || ' ' || coalesce(new.formula_sort,'') as formula, -- to ensure both formula and sort find indicator names
						coalesce(new.formula_sort,'') as formula_sort
				)
				select 
					array_agg(distinct indFORMULA.indicator_id)::int[] as formula_indicator_ids, 					
					array_agg(distinct indSORT.indicator_id)::int[] as sort_indicator_ids 
					
				into new.formula_indicator_ids,v_formula_sort_indicator_ids
				from formula_matches 
				left join p_rsf.indicators indFORMULA on formula_matches.formula::text like '%' || indFORMULA.indicator_name || '.%' -- Note '.' dot!!!!
				
				left join p_rsf.indicators indSORT on formula_matches.formula_sort::text like '%' || indSORT.indicator_name || '.%' -- Note '.' dot!!!!

				--self referential formulas are disallowed and captured above explicitly and denied, but removing this
				--to allow for self-referential sorting now that formula_sort has been implemented
				--also modified recusive CTE for <> indicator_id and to deny recusion on self-referential related indicator_ids
				--and formula_matches.indicator_id <> indFORMULA.indicator_id -- deny self-referential formulas
				group by
					formula_matches.formula_id,
					formula_matches.indicator_id;
				
			--end if;

			select ind.pfcbl_rank
			into v_formula_own_rank
			from p_rsf.indicators ind			
			where ind.indicator_id = NEW.indicator_id;

			-- Jan 2024: No!  I want to retain self-references within its parameter_ids because trigger will invalidate itself without a parameter to trigger 
			-- the evaluation when reported by the system calculator.  Retaining this will cause an updated data_unit, for example, to ensure recalculation
			--NEW.formula_indicator_ids := array_remove(NEW.indicator_id);
			
			NEW.formula_indicator_ids := array_remove(NEW.formula_indicator_ids,NULL);
			v_formula_sort_indicator_ids := array_remove(v_formula_sort_indicator_ids,NULL);
			v_formula_sort_indicator_ids := array_remove(v_formula_sort_indicator_ids,NEW.indicator_id);
						
      NEW.formula_indicator_ids := uniq(sort(coalesce(NEW.formula_indicator_ids,array[]::int[])));
			NEW.formula_indicator_ids := case when coalesce(array_length(NEW.formula_indicator_ids,1),0) = 0 
			                                  then NULL 
																				else NEW.formula_indicator_ids end;		
	
	
			v_formula_sort_indicator_ids := uniq(sort(coalesce(v_formula_sort_indicator_ids,array[]::int[])));
			v_formula_sort_indicator_ids := case when coalesce(array_length(v_formula_sort_indicator_ids,1),0) = 0 
			                                  then NULL 
																				else v_formula_sort_indicator_ids end;	
			
			
			
			
			-- if I am "sorted" (which will also de-facto group-at)
			-- and I have any parameters at the parent level, then auto-group at the parentest level defined by the sorting
			if v_formula_sort_indicator_ids is NOT NULL 
			   and exists(select * from p_rsf.indicators ind
										inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
										where array[ind.indicator_id] && v_formula_sort_indicator_ids
										  and rpc.pfcbl_rank < v_formula_own_rank) then
				 
				 select min(rpc.pfcbl_rank)
				 into v_formula_grouping_rank
				 from p_rsf.indicators ind
				 inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
				 where array[ind.indicator_id] && v_formula_sort_indicator_ids;
		  -- if I have any child parameters then auto-group at my own level
		  elseif NEW.formula_indicator_ids is NOT NULL
							and (exists(select * from p_rsf.indicators ind
														inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
														where array[ind.indicator_id] && NEW.formula_indicator_ids
															and rpc.pfcbl_rank > v_formula_own_rank) 
									 OR
									 
									 (
									 NEW.formula ~* '\.all'
									 AND
									 exists(select * from p_rsf.indicators ind
														inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
														where array[ind.indicator_id] && NEW.formula_indicator_ids
															and rpc.pfcbl_rank >= v_formula_own_rank)) 
									 ) then

					v_formula_grouping_rank := v_formula_own_rank;
			
			else 
					v_formula_grouping_rank := NULL;
			end if;
			
			NEW.formula_grouping_pfcbl_rank := v_formula_grouping_rank;
			
			
      
			select uniq(sort(array_agg(rpc.pfcbl_rank))) as formula_pfcbl_rank_range
			into NEW.formula_pfcbl_rank_range
			from (select unnest(NEW.formula_indicator_ids) as parameter_id) as params
			inner join p_rsf.indicators ind on ind.indicator_id = params.parameter_id
			inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category;


	
/* Jan 2024: Rewrote rankings to Exclude global parameters from ranking because FX calculations were getting rank of 2 when
   their only paramers are global date updates.  And all global calculations are independently calculated of rank, first.
	 This is because some rank 1 calculations were requesting fx rates that had not yet been validated due to their rank being 2
*/
/* Jul 2025: Global parameters are no longer excluded.  It doesn't gain any efficiency and it adds complexity */

/*deprecated--function based rank calculation that has some bugs with nested calculations
			select 
				frank.formula_recursive_rank,
				frank.formula_recursive_indicator_ids
			into 
				NEW.formula_calculation_rank,
				NEW.formula_indicator_id_requirements
			from p_rsf.function_get_indicator_calculation_rank(v_formula_id => NEW.formula_id,
			                                                   v_indicator_id => NEW.indicator_id,
                                                         v_formula_indicator_ids => NEW.formula_indicator_ids) as frank;
*/
   if NEW.formula_calculation_rank is NULL 
      OR
      NEW.formula_indicator_id_requirements is NULL
   then 
     NEW.formula_calculation_rank := -1;
     NEW.formula_indicator_id_requirements := array[]::int[];
   end if;
/*
      select ccr.calculation_rank,ccr.formula_indicator_id_requirements
      into 
        NEW.formula_calculation_rank,
        NEW.formula_indicator_id_requirements
      from p_rsf.compute_calculation_ranks ccr
      where ccr.formula_id = NEW.formula_id;
																												 
      NEW.formula_indicator_id_requirements := uniq(sort(NEW.formula_indicator_id_requirements));
			--raise notice '    Set formula_calculation_rank for indicator_id=% as % and requirements as %',
			--NEW.indicator_id,NEW.formula_calculation_rank,array_to_string(NEW.formula_indicator_id_requirements,',');

			select uniq(sort(array_agg(rpc.pfcbl_rank))) as formula_pfcbl_rank_range
			into NEW.formula_pfcbl_rank_range
			from (select unnest(NEW.formula_indicator_ids) as parameter_id) as params
			inner join p_rsf.indicators ind on ind.indicator_id = params.parameter_id
			inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category;

      if v_formula_own_rank <> 0
			   and NEW.formula_calculation_rank <> 1
			   and exists(select * from p_rsf.indicators ind
										where ind.indicator_id = NEW.indicator_id
											and ind.data_type = 'currency_ratio')
      then
				raise exception 'Failed to update formula for indicator_id=% with formula=% having a rank=%
				                 because currency_ratio indicators must resolve a formula_calculation_rank=1.
												 Currency ratio calculations are expected to set this formula:
												 get_IFC_FX_rate(exchange_rate_date=global_reporting_quarter_end_date.current.reporteddate,
												                 currency_code_ratio=%.current.unit)',
												 NEW.indicator_id,
												 NEW.formula,
												 NEW.formula_calculation_rank,
												 (select indicator_name from p_rsf.indicators where indicator_id=NEW.indicator_id);
			
			end if;
			--raise notice 'Done for indicator_id=%', NEW.indicator_id;
		  
			if exists(select * from p_rsf.indicators ind
								where ind.indicator_id = NEW.indicator_id
									and ind.data_type = 'currency_ratio')			
			then
				NEW.computation_priority_rank := 1;
			end if;
*/			
	RETURN NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for set_indicator_is_calculated
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_indicator_is_calculated"();
CREATE FUNCTION "p_rsf"."set_indicator_is_calculated"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE ind_id int;
BEGIN
	
	if (TG_OP in ('new','update'))
	then
		ind_id := NEW.indicator_id;
	else 
		ind_id := OLD.indicator_id;
	end if;
	
	update p_rsf.indicators ind
	set is_calculated = exists(select * 
														 from p_rsf.indicator_formulas indf
														 where indf.indicator_id = ind.indicator_id
														   and indf.is_primary_default = true)
  where ind.indicator_id = ind_id;
			
	RETURN NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for set_limit
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_limit"(float4);
CREATE FUNCTION "p_rsf"."set_limit"(float4)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'set_limit'
  LANGUAGE c VOLATILE STRICT
  COST 1;

-- ----------------------------
-- Function structure for set_new_label_id
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."set_new_label_id"();
CREATE FUNCTION "p_rsf"."set_new_label_id"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE lif text;
BEGIN

	if (NEW.label_id is NULL) then
		if (TG_ARGV[0]='indicators') then
			lif := 'indicators';
		elseif (TG_ARGV[0]='indicator_options_group_keys') then
			lif := 'options-' || NEW.options_group_id::text;
    elseif (TG_ARGV[0]='indicator_formulas') then
      lif := TG_ARGV[0];
    elseif (TG_ARGV[0]='indicator_check_formulas') then
      lif := TG_ARGV[0];
		end if;
		
		with new_id as MATERIALIZED (insert into p_rsf.label_ids(label_id,label_id_group) values(DEFAULT,lif) returning label_id)
		select label_id into NEW.label_id
		from new_id;
	end if;
	
	return NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for show_limit
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."show_limit"();
CREATE FUNCTION "p_rsf"."show_limit"()
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'show_limit'
  LANGUAGE c STABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for show_trgm
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."show_trgm"(text);
CREATE FUNCTION "p_rsf"."show_trgm"(text)
  RETURNS "pg_catalog"."_text" AS '$libdir/pg_trgm', 'show_trgm'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for similarity
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."similarity"(text, text);
CREATE FUNCTION "p_rsf"."similarity"(text, text)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'similarity'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for similarity_dist
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."similarity_dist"(text, text);
CREATE FUNCTION "p_rsf"."similarity_dist"(text, text)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'similarity_dist'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for similarity_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."similarity_op"(text, text);
CREATE FUNCTION "p_rsf"."similarity_op"(text, text)
  RETURNS "pg_catalog"."bool" AS '$libdir/pg_trgm', 'similarity_op'
  LANGUAGE c STABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for strict_word_similarity
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."strict_word_similarity"(text, text);
CREATE FUNCTION "p_rsf"."strict_word_similarity"(text, text)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'strict_word_similarity'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for strict_word_similarity_commutator_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."strict_word_similarity_commutator_op"(text, text);
CREATE FUNCTION "p_rsf"."strict_word_similarity_commutator_op"(text, text)
  RETURNS "pg_catalog"."bool" AS '$libdir/pg_trgm', 'strict_word_similarity_commutator_op'
  LANGUAGE c STABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for strict_word_similarity_dist_commutator_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."strict_word_similarity_dist_commutator_op"(text, text);
CREATE FUNCTION "p_rsf"."strict_word_similarity_dist_commutator_op"(text, text)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'strict_word_similarity_dist_commutator_op'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for strict_word_similarity_dist_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."strict_word_similarity_dist_op"(text, text);
CREATE FUNCTION "p_rsf"."strict_word_similarity_dist_op"(text, text)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'strict_word_similarity_dist_op'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for strict_word_similarity_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."strict_word_similarity_op"(text, text);
CREATE FUNCTION "p_rsf"."strict_word_similarity_op"(text, text)
  RETURNS "pg_catalog"."bool" AS '$libdir/pg_trgm', 'strict_word_similarity_op'
  LANGUAGE c STABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for validate_global_indicator_subscriptions
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."validate_global_indicator_subscriptions"();
CREATE FUNCTION "p_rsf"."validate_global_indicator_subscriptions"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  if (NEW.rsf_program_id = 0
	    and not exists(select * from p_rsf.indicators ind
			               where ind.indicator_id = NEW.indicator_id
										   and ind.data_category = 'global')) then
	  raise exception 'Global program can only subscribe to global indicators';
		return NULL;
	elseif (NEW.rsf_program_id <> 0
		       and exists(select * from p_rsf.indicators ind
			                where ind.indicator_id = NEW.indicator_id
										    and ind.data_category = 'global')) then
	  raise exception 'Only Global program can subscribe to global indicators';
		return NULL;
	end if;
	
	return NEW;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for validate_sys_indicator_subscriptions
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."validate_sys_indicator_subscriptions"();
CREATE FUNCTION "p_rsf"."validate_sys_indicator_subscriptions"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

	if (exists(select * from p_rsf.rsf_programs where rsf_program_id = OLD.rsf_program_id)
	    and exists(select * 
	           from p_rsf.indicators ind
						 where ind.indicator_id = OLD.indicator_id
						   and ind.is_system = true)) then 
	  raise exception 'Programs must subscribe to system indicators; they cannot be unsubscribed.  Delete rsf_program_id for this action';
		return NULL;
  end if;		
	return OLD;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for word_similarity
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."word_similarity"(text, text);
CREATE FUNCTION "p_rsf"."word_similarity"(text, text)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'word_similarity'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for word_similarity_commutator_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."word_similarity_commutator_op"(text, text);
CREATE FUNCTION "p_rsf"."word_similarity_commutator_op"(text, text)
  RETURNS "pg_catalog"."bool" AS '$libdir/pg_trgm', 'word_similarity_commutator_op'
  LANGUAGE c STABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for word_similarity_dist_commutator_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."word_similarity_dist_commutator_op"(text, text);
CREATE FUNCTION "p_rsf"."word_similarity_dist_commutator_op"(text, text)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'word_similarity_dist_commutator_op'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for word_similarity_dist_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."word_similarity_dist_op"(text, text);
CREATE FUNCTION "p_rsf"."word_similarity_dist_op"(text, text)
  RETURNS "pg_catalog"."float4" AS '$libdir/pg_trgm', 'word_similarity_dist_op'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;

-- ----------------------------
-- Function structure for word_similarity_op
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."word_similarity_op"(text, text);
CREATE FUNCTION "p_rsf"."word_similarity_op"(text, text)
  RETURNS "pg_catalog"."bool" AS '$libdir/pg_trgm', 'word_similarity_op'
  LANGUAGE c STABLE STRICT
  COST 1;

-- ----------------------------
-- View structure for util_reporting_cohort_times_by_entity
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."util_reporting_cohort_times_by_entity";
CREATE VIEW "p_rsf"."util_reporting_cohort_times_by_entity" AS  SELECT rsf_program_id,
    sys_name,
    reporting_rsf_pfcbl_id,
    sum(total_time_sec) AS total_total_time,
    avg(total_time_sec) AS avg_total_time,
    count(*) AS count_total_files,
    avg(parse_time_sec) AS avg_parse_time,
    avg(process_time_sec) AS avg_process_time,
    avg(upload_time_sec) AS avg_upload_time,
    avg(backup_time_sec) AS avg_backup_time,
    sum(parse_time_sec) AS total_parse_time,
    sum(process_time_sec) AS total_process_time,
    sum(upload_time_sec) AS total_upload_time,
    sum(backup_time_sec) AS total_backup_time
   FROM p_rsf.util_reporting_cohort_info_process_times rct
  GROUP BY sys_name, reporting_rsf_pfcbl_id, rsf_program_id
  ORDER BY sys_name;

-- ----------------------------
-- View structure for view_indicator_labels
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_indicator_labels";
CREATE VIEW "p_rsf"."view_indicator_labels" AS  SELECT ind.indicator_id,
    ind.indicator_name,
    lab.label_id,
    lab.label_key,
    lab.primary_label AS label,
    true AS is_primary
   FROM p_rsf.labels lab
     JOIN p_rsf.indicators ind ON ind.label_id = lab.label_id
UNION ALL
 SELECT ind.indicator_id,
    ind.indicator_name,
    lab.label_id,
    lab.label_key,
    secondary_label.secondary_label AS label,
    false AS is_primary
   FROM p_rsf.labels lab
     JOIN p_rsf.indicators ind ON ind.label_id = lab.label_id
     JOIN LATERAL unnest(lab.secondary_labels) secondary_label(secondary_label) ON true
UNION ALL
 SELECT ind.indicator_id,
    ind.indicator_name,
    ind.label_id,
    'SYS'::character varying AS label_key,
    ind.indicator_name AS label,
    NULL::boolean AS is_primary
   FROM p_rsf.indicators ind;

-- ----------------------------
-- View structure for view_rsf_data_checks_archive_eligible
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_data_checks_archive_eligible";
CREATE VIEW "p_rsf"."view_rsf_data_checks_archive_eligible" AS  SELECT rdc.evaluation_id,
    timeofday()::timestamp with time zone AS archive_time,
    rdc.archive_sys_name AS sys_name,
    rdc.rsf_pfcbl_id,
    rdc.indicator_id,
    rdc.indicator_check_id,
    rdc.check_formula_id,
    rdc.check_asof_date,
    rdc.check_status,
    rdc.status_time,
    rdc.check_status_user_id,
    rdc.check_status_comment,
    rdc.check_message,
    rdc.consolidated_from_indicator_id,
    rdc.consolidated_from_indicator_check_id,
    rdc.data_sys_flags,
    rdc.data_value_unit,
    rdc.data_id
   FROM p_rsf.rsf_data_checks rdc
     JOIN p_rsf.view_account_info vai ON vai.account_id = rdc.check_status_user_id
     LEFT JOIN p_rsf.indicator_check_guidance icg ON icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
  WHERE rdc.archive_sys_name IS NOT NULL AND rdc.data_value_unit IS NOT NULL AND rdc.check_status_comment IS DISTINCT FROM icg.guidance AND vai.is_system_account = false;

-- ----------------------------
-- View structure for view_options_labels
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_options_labels";
CREATE VIEW "p_rsf"."view_options_labels" AS  SELECT iog.options_group_id,
    iog.options_group_name,
    iog.options_group_data_type,
    ogk.options_group_key,
    ogk.label_id,
    lab.label_key,
    lab.primary_label AS label,
    true AS is_primary
   FROM p_rsf.indicator_options_groups iog
     JOIN p_rsf.indicator_options_group_keys ogk ON ogk.options_group_id = iog.options_group_id
     JOIN p_rsf.labels lab ON lab.label_id = ogk.label_id
UNION ALL
 SELECT iog.options_group_id,
    iog.options_group_name,
    iog.options_group_data_type,
    ogk.options_group_key,
    ogk.label_id,
    lab.label_key,
    secondary_label.secondary_label AS label,
    false AS is_primary
   FROM p_rsf.indicator_options_groups iog
     JOIN p_rsf.indicator_options_group_keys ogk ON ogk.options_group_id = iog.options_group_id
     JOIN p_rsf.labels lab ON lab.label_id = ogk.label_id
     JOIN LATERAL unnest(lab.secondary_labels) secondary_label(secondary_label) ON true
UNION ALL
 SELECT iog.options_group_id,
    iog.options_group_name,
    iog.options_group_data_type,
    ogk.options_group_key,
    ogk.label_id,
    'SYS'::character varying AS label_key,
    ogk.options_group_key AS label,
    NULL::boolean AS is_primary
   FROM p_rsf.indicator_options_groups iog
     JOIN p_rsf.indicator_options_group_keys ogk ON ogk.options_group_id = iog.options_group_id;

-- ----------------------------
-- View structure for view_rsf_program_facility_template_header_actions
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_program_facility_template_header_actions";
CREATE VIEW "p_rsf"."view_rsf_program_facility_template_header_actions" AS  SELECT headers.rsf_pfcbl_id,
    headers.template_id,
    fth_a.header_id,
    fth_a."SYSNAME",
    fth_a.template_name,
    fth_a.template_header_sheet_name AS template_header_section_name,
    fth_a.template_header,
    fth_a.action,
    fth_a.comment,
    fth_a.map_indicator_id,
    fth_a.indicator_name,
    fth_a.map_formula_id,
    fth_a.calculation_formula,
    fth_a.map_check_formula_id,
    fth_a.check_formula,
    headers.action_level
   FROM ( SELECT DISTINCT ON (ft.from_rsf_pfcbl_id, fth.template_id, fth.template_header_full_normalized) ft.from_rsf_pfcbl_id AS rsf_pfcbl_id,
            ft.to_family_rsf_pfcbl_id,
            fth.template_id,
            fth.template_header_full_normalized,
            ft.to_pfcbl_category AS action_level
           FROM p_rsf.view_rsf_pfcbl_id_family_tree ft
             JOIN p_rsf.rsf_program_facility_template_headers fth ON fth.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
          WHERE (ft.from_pfcbl_category::text = ANY (ARRAY['global'::character varying, 'program'::character varying, 'facility'::character varying]::text[])) AND ft.to_pfcbl_rank <= ft.from_pfcbl_rank
          ORDER BY ft.from_rsf_pfcbl_id, fth.template_id, fth.template_header_full_normalized, ft.to_pfcbl_rank DESC) headers
     JOIN p_rsf.view_rsf_program_facility_template_headers fth_a ON fth_a.rsf_pfcbl_id = headers.to_family_rsf_pfcbl_id AND fth_a.template_id = headers.template_id AND fth_a.template_header_full_normalized = headers.template_header_full_normalized;

-- ----------------------------
-- View structure for view_rsf_setup_programs_indicators
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_programs_indicators";
CREATE VIEW "p_rsf"."view_rsf_setup_programs_indicators" AS  SELECT ids.rsf_program_id,
    ids.rsf_facility_id,
    ids.rsf_pfcbl_id,
    ind.indicator_id,
    sn.sys_name,
    ind.indicator_name,
    pfi.is_subscribed AS monitored,
    pfi.formula_id,
    indf.formula_title,
    pfi.is_auto_subscribed,
    pfi.sort_preference,
    pfi.subscription_comments,
    pfi.comments_user_id,
    ids.pfcbl_category_rank,
    pfi.options_group_id,
    pfi.formula_calculation_unit
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.rsf_setup_indicators pfi ON pfi.rsf_pfcbl_id = ids.rsf_pfcbl_id
     JOIN p_rsf.indicators ind ON ind.indicator_id = pfi.indicator_id
     JOIN LATERAL ( SELECT nai.sys_name
           FROM p_rsf.rsf_data_current_names_and_ids nai
          WHERE nai.rsf_pfcbl_id = ids.rsf_pfcbl_id
          ORDER BY nai.reporting_asof_date
         LIMIT 1) sn ON true
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.formula_id = pfi.formula_id
  WHERE ids.rsf_program_id <> 0 AND ind.is_system = false AND ind.data_category::text <> 'global'::text AND (ind.indicator_name::text ~ '^rsf_'::text) = false
  ORDER BY ids.rsf_program_id, sn.sys_name, pfi.is_subscribed DESC, ind.indicator_name;

-- ----------------------------
-- View structure for compute_calculation_ranks
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."compute_calculation_ranks";
CREATE VIEW "p_rsf"."compute_calculation_ranks" AS  WITH RECURSIVE ranks AS (
         SELECT indf.formula_id,
            indf.indicator_id,
            iparameter_id.iparameter_id AS parameter_id,
            1 AS crank,
            false AS here,
            ind.data_category,
            ind.data_type
           FROM p_rsf.indicator_formulas indf
             JOIN p_rsf.indicators ind ON ind.indicator_id = indf.indicator_id
             LEFT JOIN LATERAL unnest(indf.formula_indicator_ids) iparameter_id(iparameter_id) ON iparameter_id.iparameter_id IS DISTINCT FROM indf.indicator_id
        UNION
         SELECT ranks_1.formula_id,
            ind.indicator_id,
                CASE
                    WHEN present.indicator_id IS NOT NULL THEN NULL::integer
                    ELSE iparameter_id.iparameter_id
                END AS parameter_id,
            ranks_1.crank + 1 AS crank,
            present.indicator_id IS NOT NULL AS here,
            ranks_1.data_category,
            ranks_1.data_type
           FROM ranks ranks_1
             JOIN p_rsf.indicators ind ON ind.indicator_id = ranks_1.parameter_id
             JOIN p_rsf.indicator_formulas indf ON indf.indicator_id = ind.indicator_id AND indf.formula_id IS DISTINCT FROM ranks_1.formula_id
             LEFT JOIN LATERAL unnest(indf.formula_indicator_ids) iparameter_id(iparameter_id) ON iparameter_id.iparameter_id IS DISTINCT FROM indf.indicator_id
             LEFT JOIN p_rsf.indicator_formula_parameters present ON present.indicator_id = ranks_1.indicator_id AND present.indicator_id = iparameter_id.iparameter_id
          WHERE ranks_1.crank < 25 AND
                CASE
                    WHEN ranks_1.data_category::text <> 'global'::text AND ind.data_category::text = 'global'::text THEN false
                    ELSE true
                END
        )
 SELECT formula_id,
    data_category,
    data_type,
    max(crank) AS calculation_rank,
    array_remove(array_agg(DISTINCT parameter_id ORDER BY parameter_id), NULL::integer) AS formula_indicator_id_requirements,
    sum(here::integer) AS nested_ranks
   FROM ranks
  GROUP BY formula_id, data_category, data_type;

-- ----------------------------
-- View structure for view_rsf_setup_check_config
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_check_config";
CREATE VIEW "p_rsf"."view_rsf_setup_check_config" AS  SELECT ids.rsf_pfcbl_id,
    ids.pfcbl_category,
    scc.for_indicator_id,
    ic.indicator_check_id,
    ids.rsf_program_id,
    ids.rsf_facility_id,
    true AS is_subscribed,
    false AS is_unsubscribed,
    false AS is_auto_subscribed,
    COALESCE(scc.config_auto_resolve, ic.auto_resolve_system_check, false) AS config_auto_resolve,
    COALESCE(scc.config_check_class, ic.check_class::text) AS config_check_class,
        CASE
            WHEN ic.variance_tolerance_allowed IS FALSE THEN NULL::numeric
            ELSE COALESCE(scc.config_threshold, 0::numeric)
        END AS config_threshold,
    scc.config_comments,
    scc.comments_user_id,
    ic.check_name,
    ind.indicator_name,
    ids.pfcbl_category_rank = ind.pfcbl_rank AS filter_matched_pfcbl_indicators,
    ids.pfcbl_category_rank = ind.pfcbl_rank AND ids.pfcbl_category_rank <= 2 OR ids.pfcbl_category_rank = 2 AND ind.pfcbl_rank >= 2 AS filter_category_manager,
        CASE
            WHEN ind.pfcbl_rank = 0 THEN 0
            WHEN ind.pfcbl_rank = 1 THEN ids.rsf_program_id
            ELSE ids.rsf_facility_id
        END AS category_manager_rsf_pfcbl_id
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.rsf_setup_checks_config scc ON scc.rsf_pfcbl_id = ids.rsf_facility_id OR scc.rsf_pfcbl_id = ids.rsf_program_id OR scc.rsf_pfcbl_id = 0
     JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = scc.indicator_check_id
     JOIN p_rsf.indicators ind ON ind.indicator_id = scc.for_indicator_id;

-- ----------------------------
-- View structure for view_rsf_setup_programs_data
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_programs_data";
CREATE VIEW "p_rsf"."view_rsf_setup_programs_data" AS  SELECT dense_rank() OVER (ORDER BY ids.created_in_reporting_asof_date, ids.rsf_program_id) AS program_sequence,
    ids.rsf_program_id,
    ids.rsf_facility_id,
    ids.rsf_pfcbl_id,
    sn.sys_name,
    rpr.reporting_asof_date,
    ids.created_in_reporting_asof_date = rdc.reporting_asof_date AS is_creation_asof_date,
    rdc.data_id IS NULL AS entity_never_reported,
    ind.data_category,
    ind.indicator_name,
    ind.is_calculated,
    ind.indicator_id,
    indf.formula_id,
        CASE
            WHEN rdc.data_id IS NULL AND ind.is_calculated THEN '{UNCALCULATED}'::text
            WHEN rdc.data_id IS NULL THEN '{MISSING}'::text
            WHEN rdc.data_value IS NULL AND ind.is_calculated THEN '{CALCULATED:BLANK}'::text
            WHEN rdc.data_value IS NULL THEN '{BLANK}'::text
            ELSE rdc.data_value
        END AS data_value,
        CASE
            WHEN rdc.data_id IS NULL THEN NULL::text
            ELSE rdc.data_unit
        END AS data_unit,
    rdc.data_id
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.view_rsf_setup_indicator_subscriptions sis ON sis.rsf_pfcbl_id = ids.rsf_pfcbl_id
     JOIN p_rsf.indicators ind ON ind.indicator_id = sis.indicator_id
     JOIN p_rsf.rsf_pfcbl_reporting rpr ON rpr.rsf_pfcbl_id = ids.rsf_pfcbl_id
     JOIN LATERAL ( SELECT nai.sys_name
           FROM p_rsf.rsf_data_current_names_and_ids nai
          WHERE nai.rsf_pfcbl_id = ids.rsf_pfcbl_id AND nai.reporting_asof_date <= rpr.reporting_asof_date
          ORDER BY nai.reporting_asof_date DESC
         LIMIT 1) sn ON true
     LEFT JOIN p_rsf.rsf_data_current rdc ON rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id AND rdc.indicator_id = ind.indicator_id AND rdc.reporting_asof_date = rpr.reporting_asof_date
     LEFT JOIN p_rsf.rsf_data rd ON rd.data_id = rdc.data_id
     LEFT JOIN p_rsf.reporting_cohorts rc ON rc.reporting_cohort_id = rd.reporting_cohort_id
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.formula_id = sis.formula_id
  WHERE (ids.pfcbl_category::text = ANY (ARRAY['program'::character varying::text, 'facility'::character varying::text, 'client'::character varying::text])) AND (sis.is_subscribed = true OR rd.data_id IS NOT NULL) AND sis.is_unsubscribed IS NOT TRUE AND (ids.pfcbl_category::text = 'program'::text OR (EXISTS ( SELECT rd_1.data_id,
            rd_1.rsf_pfcbl_id,
            rd_1.indicator_id,
            rd_1.reporting_asof_date,
            rd_1.reporting_cohort_id,
            rd_1.data_value,
            rd_1.data_unit,
            rd_1.data_submitted,
            rd_1.data_source_row_id,
            rd_1.data_sys_flags,
            rd_1.data_sys_source,
            idind.indicator_id,
            idind.indicator_name,
            idind.indicator_sys_category,
            idind.data_category,
            idind.data_type,
            idind.data_unit,
            idind.default_value,
            idind.definition,
            idind.label_id,
            idind.indicator_options_group_id,
            idind.indicator_options_group_allows_blanks,
            idind.indicator_options_group_allows_multiples,
            idind.is_calculated,
            idind.is_system,
            idind.is_data_unit,
            idind.is_system_calculated,
            idind.is_setup,
            idind.modification_time,
            idind.version_number,
            idind.is_static_nonreporting,
            idind.default_subscription,
            idind.is_periodic_or_flow_reporting,
            idind.classification,
            idind.sort_preference,
            idind.created_by_user_id,
            idind.modified_by_user_id,
            idind.pfcbl_rank
           FROM p_rsf.rsf_data rd_1
             JOIN p_rsf.indicators idind ON idind.indicator_id = rd_1.indicator_id AND idind.indicator_sys_category::text = 'id'::text
          WHERE rd_1.rsf_pfcbl_id = ids.rsf_pfcbl_id AND rd_1.data_value IS NOT NULL))) AND ind.is_system = false AND (ind.is_periodic_or_flow_reporting IS FALSE OR NOT rdc.reporting_asof_date IS DISTINCT FROM rpr.reporting_asof_date) AND rc.is_calculated_cohort IS FALSE AND (ind.indicator_name::text ~ '^rsf'::text) = false
  ORDER BY (dense_rank() OVER (ORDER BY ids.created_in_reporting_asof_date, ids.rsf_program_id)), ids.pfcbl_category_rank, ids.rsf_program_id, ids.rsf_facility_id, ids.rsf_client_id, ind.indicator_name, (COALESCE(rdc.reporting_asof_date, ids.created_in_reporting_asof_date));

-- ----------------------------
-- View structure for view_reporting_cohort_file_name
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_reporting_cohort_file_name";
CREATE VIEW "p_rsf"."view_reporting_cohort_file_name" AS  SELECT sn.sys_name,
    sn.pfcbl_name,
    rc.reporting_rsf_pfcbl_id,
    rc.reporting_asof_date,
    rc.reporting_cohort_id,
    rc.reporting_time,
    rc.reporting_user_id,
    rc."!dep-source_name" AS source_name,
    regexp_replace(rc."!dep-source_name", '^[^[:alpha:]]+'::text, ''::text) AS file_name,
    client.reporting_date,
        CASE
            WHEN COALESCE(rc.reporting_asof_date >= client.reporting_date, false) THEN dense_rank() OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, (COALESCE(rc.reporting_asof_date >= client.reporting_date, false)) ORDER BY rc.reporting_asof_date)
            ELSE 0::bigint
        END AS reporting_rank,
    NULLIF(
        CASE
            WHEN count(*) OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, rc.reporting_asof_date) = 1 THEN NULL::bigint
            ELSE dense_rank() OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, rc.reporting_asof_date ORDER BY rc.reporting_time) - 1
        END, 0) AS sequence_rank,
    concat('#',
        CASE
            WHEN COALESCE(rc.reporting_asof_date >= client.reporting_date, false) THEN dense_rank() OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, (COALESCE(rc.reporting_asof_date >= client.reporting_date, false)) ORDER BY rc.reporting_asof_date)
            ELSE 0::bigint
        END, '.'::text || NULLIF(
        CASE
            WHEN count(*) OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, rc.reporting_asof_date) = 1 THEN NULL::bigint
            ELSE dense_rank() OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, rc.reporting_asof_date ORDER BY rc.reporting_time) - 1
        END, 0)) AS name_prefix,
    (concat('#',
        CASE
            WHEN COALESCE(rc.reporting_asof_date >= client.reporting_date, false) THEN dense_rank() OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, (COALESCE(rc.reporting_asof_date >= client.reporting_date, false)) ORDER BY rc.reporting_asof_date)
            ELSE 0::bigint
        END, '.'::text || NULLIF(
        CASE
            WHEN count(*) OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, rc.reporting_asof_date) = 1 THEN NULL::bigint
            ELSE dense_rank() OVER (PARTITION BY rc.reporting_rsf_pfcbl_id, rc.reporting_asof_date ORDER BY rc.reporting_time) - 1
        END, 0)) || ' '::text) || regexp_replace(rc."!dep-source_name", '^[^[:alpha:]]+'::text, ''::text) AS prefix_file_name
   FROM p_rsf.reporting_cohorts rc
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
     LEFT JOIN p_rsf.rsf_pfcbl_ids client_ids ON client_ids.rsf_facility_id = rc.reporting_rsf_pfcbl_id AND client_ids.pfcbl_category::text = 'client'::text
     LEFT JOIN LATERAL ( SELECT min(rdc.data_value::date) AS reporting_date
           FROM p_rsf.rsf_data_current rdc
          WHERE rdc.rsf_pfcbl_id = client_ids.rsf_pfcbl_id AND rdc.indicator_id = (( SELECT ind.indicator_id
                   FROM p_rsf.indicators ind
                  WHERE ind.data_category::text = 'client'::text AND ind.indicator_sys_category::text = 'reporting_required_start_date'::text))) client ON true
  WHERE rc."!dep-parent_reporting_cohort_id" IS NULL
  ORDER BY (rc.reporting_rsf_pfcbl_id = 0), rc.reporting_rsf_pfcbl_id, rc.reporting_asof_date, rc.reporting_time;

-- ----------------------------
-- View structure for view_rsf_pfcbl_id_family_parents
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pfcbl_id_family_parents";
CREATE VIEW "p_rsf"."view_rsf_pfcbl_id_family_parents" AS  SELECT ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    fids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    fids.pfcbl_category AS to_pfcbl_category,
    fids.pfcbl_category_rank AS to_pfcbl_rank
   FROM p_rsf.rsf_pfcbl_ids ids
     CROSS JOIN p_rsf.rsf_pfcbl_ids fids
  WHERE fids.rsf_pfcbl_id = ANY (ARRAY[ids.rsf_loan_id, ids.rsf_borrower_id, ids.rsf_client_id, ids.rsf_facility_id, ids.rsf_program_id, 0]);

-- ----------------------------
-- View structure for view_rsf_setup_indicator_subscriptions
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_indicator_subscriptions";
CREATE VIEW "p_rsf"."view_rsf_setup_indicator_subscriptions" AS  SELECT ids.rsf_pfcbl_id,
    ids.pfcbl_category,
    ids.pfcbl_category_rank AS pfcbl_rank,
    ind.indicator_name,
    ind.indicator_id,
    ind.data_category,
    ind.pfcbl_rank AS data_category_rank,
    ind.data_type,
    ind.default_value,
    ind.data_unit AS default_unit,
    ind.indicator_sys_category,
    ind.is_system AS is_system_indicator,
    indf.formula_id,
        CASE
            WHEN pfi.formula_calculation_unit IS NOT NULL THEN pfi.formula_calculation_unit
            WHEN indf.formula_id IS NOT NULL THEN ind.data_unit::text
            ELSE NULL::text
        END AS formula_calculation_unit,
    indf.formula_calculation_rank,
    indf.formula_id IS NOT NULL AS is_calculated,
    COALESCE(pfi.is_subscribed, false) AS is_subscribed,
    COALESCE(pfi.is_subscribed IS FALSE, false) AS is_unsubscribed,
    COALESCE(pfi.is_auto_subscribed, false) AS is_auto_subscribed,
    ids.pfcbl_category_rank = ind.pfcbl_rank AS filter_matched_pfcbl_indicators,
    ids.pfcbl_category_rank = ind.pfcbl_rank AND ids.pfcbl_category_rank <= 2 OR ids.pfcbl_category_rank = 2 AND ind.pfcbl_rank >= 2 AS filter_category_manager,
    pfi.subscription_comments,
    pfi.comments_user_id,
        CASE
            WHEN ind.pfcbl_rank = 0 THEN 0
            WHEN ind.pfcbl_rank = 1 THEN ids.rsf_program_id
            ELSE ids.rsf_facility_id
        END AS category_manager_rsf_pfcbl_id,
    ids.created_in_reporting_asof_date,
    ids.deactivated_in_reporting_asof_date
   FROM p_rsf.rsf_pfcbl_ids ids
     CROSS JOIN p_rsf.indicators ind
     LEFT JOIN p_rsf.rsf_setup_indicators pfi ON (pfi.rsf_pfcbl_id = ids.rsf_facility_id OR pfi.rsf_pfcbl_id = ids.rsf_program_id OR pfi.rsf_pfcbl_id = 0) AND pfi.indicator_id = ind.indicator_id
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.indicator_id = ind.indicator_id AND (pfi.indicator_id IS NULL AND indf.is_primary_default IS TRUE OR NOT pfi.formula_id IS DISTINCT FROM indf.formula_id);

-- ----------------------------
-- View structure for compute_check_grouping
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."compute_check_grouping";
CREATE VIEW "p_rsf"."compute_check_grouping" AS  SELECT check_formula_id,
    indicator_check_id,
    array_to_string(array_agg(DISTINCT computation_code ORDER BY computation_code), '-'::text) AS computation_code,
    bit_or(
        CASE
            WHEN computation_code = 'L'::text THEN 2::double precision ^ 0::double precision
            WHEN computation_code = 'B'::text THEN 2::double precision ^ 1::double precision
            WHEN computation_code = ANY (ARRAY['C'::text, 'F'::text, 'F>C'::text, 'C>F'::text]) THEN 2::double precision ^ 2::double precision
            WHEN computation_code = 'P'::text THEN 2::double precision ^ 2::double precision
            WHEN computation_code = 'B>L'::text THEN (((2::double precision ^ 4::double precision)::integer | (2::double precision ^ 1::double precision)::integer) | (2::double precision ^ 0::double precision)::integer)::double precision
            WHEN computation_code = ANY (ARRAY['C>L'::text, 'F>L'::text]) THEN (((2::double precision ^ 5::double precision)::integer | (2::double precision ^ 0::double precision)::integer) | (2::double precision ^ 2::double precision)::integer)::double precision
            WHEN computation_code = 'P>L'::text THEN (((2::double precision ^ 6::double precision)::integer | (2::double precision ^ 0::double precision)::integer) | (2::double precision ^ 3::double precision)::integer)::double precision
            WHEN computation_code = ANY (ARRAY['C>B'::text, 'F>B'::text]) THEN (((2::double precision ^ 7::double precision)::integer | (2::double precision ^ 2::double precision)::integer) | (2::double precision ^ 1::double precision)::integer)::double precision
            WHEN computation_code = 'P>B'::text THEN (((2::double precision ^ 8::double precision)::integer | (2::double precision ^ 3::double precision)::integer) | (2::double precision ^ 1::double precision)::integer)::double precision
            WHEN computation_code = ANY (ARRAY['P>C'::text, 'P>F'::text]) THEN (((2::double precision ^ 9::double precision)::integer | (2::double precision ^ 2::double precision)::integer) | (2::double precision ^ 3::double precision)::integer)::double precision
            ELSE 0::double precision
        END::integer) AS computation_group
   FROM p_rsf.compute_check_to_parameter_categories ccp
  GROUP BY check_formula_id, indicator_check_id;

-- ----------------------------
-- View structure for view_current_entity_names_and_ids
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_current_entity_names_and_ids";
CREATE VIEW "p_rsf"."view_current_entity_names_and_ids" AS  SELECT DISTINCT ON (ids.rsf_pfcbl_id) ids.rsf_program_id,
    ids.rsf_pfcbl_id,
    ids.created_in_reporting_asof_date,
    ids.pfcbl_category,
    nai.sys_name,
    nai.name,
    nai.nickname,
    nai.id,
    nai.rank_id,
    concat(COALESCE(nai.nickname, nai.name, 'RANK'::text || nai.rank_id, ('MISSING '::text || upper(ids.pfcbl_category::text)) || ' NAME'::text), ' (', COALESCE(nai.id, 'SYSID'::text || ids.rsf_pfcbl_id), ')') AS rsf_full_name,
    COALESCE(nai.nickname, nai.name, 'RANK'::text || nai.rank_id, ('MISSING '::text || upper(ids.pfcbl_category::text)) || ' NAME'::text) AS rsf_name
   FROM p_rsf.rsf_pfcbl_ids ids
     LEFT JOIN p_rsf.rsf_data_current_names_and_ids nai ON nai.rsf_pfcbl_id = ids.rsf_pfcbl_id
  ORDER BY ids.rsf_pfcbl_id, nai.reporting_asof_date DESC NULLS LAST;

-- ----------------------------
-- View structure for util_facility_setup_indicators_not_used_as_parameters
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."util_facility_setup_indicators_not_used_as_parameters";
CREATE VIEW "p_rsf"."util_facility_setup_indicators_not_used_as_parameters" AS  SELECT indicator_id,
    indicator_name
   FROM p_rsf.indicators ind
  WHERE data_category::text = 'facility'::text AND is_system = false AND NOT ((EXISTS ( SELECT cfp.indicator_check_id,
            cfp.check_formula_id,
            cfp.for_pfcbl_category,
            cfp.check_grouping_pfcbl_category,
            cfp.check_grouping_pfcbl_rank,
            cfp.parameter_indicator_id,
            cfp.parameter_pfcbl_category,
            cfp.parameter_pfcbl_rank,
            cfp.parameter_pfcbl_hierarchy,
            cfp.is_calculation_trigger_parameter,
            cfp.parameter_trigger_by_reporting
           FROM p_rsf.indicator_check_formula_parameters cfp
          WHERE cfp.parameter_indicator_id = ind.indicator_id)) OR (EXISTS ( SELECT ifp.indicator_id,
            ifp.calculate_pfcbl_category,
            ifp.calculate_grouping_pfcbl_category,
            ifp.calculate_grouping_pfcbl_rank,
            ifp.parameter_indicator_id,
            ifp.parameter_pfcbl_category,
            ifp.parameter_pfcbl_rank,
            ifp.parameter_pfcbl_hierarchy,
            ifp.parameter_is_current,
            ifp.parameter_is_previous,
            ifp.parameter_is_info,
            ifp.parameter_is_all,
            ifp.parameter_trigger_by_reporting,
            ifp.parameter_data_type,
            ifp.formula_id
           FROM p_rsf.indicator_formula_parameters ifp
          WHERE ifp.parameter_indicator_id = ind.indicator_id)) OR (EXISTS ( SELECT indf.indicator_id,
            indf.formula,
            indf.formula_sort,
            indf.overwrite,
            indf.formula_indicator_ids,
            indf.formula_indicator_id_requirements,
            indf.formula_indicator_id_dependents,
            indf.formula_calculation_rank,
            indf.formula_grouping_pfcbl_rank,
            indf.formula_pfcbl_rank_range,
            indf."dep-formula_calculated_by_indicator_id",
            indf.perform_calculation_by_row,
            indf.modification_time,
            indf.formula_fx_date,
            indf.computation_group,
            indf.computation_priority_rank,
            indf.formula_unit_set_by_indicator_id,
            indf.formula_id,
            indf.formula_title,
            indf.is_primary_default,
            indf.formula_notes,
            indf.modified_by_user_id
           FROM p_rsf.indicator_formulas indf
          WHERE indf.indicator_id = ind.indicator_id)));

-- ----------------------------
-- View structure for compute_check_to_parameter_rsf_pfcbl_ids
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."compute_check_to_parameter_rsf_pfcbl_ids";
CREATE VIEW "p_rsf"."compute_check_to_parameter_rsf_pfcbl_ids" AS  SELECT ids.rsf_pfcbl_id AS from_check_rsf_pfcbl_id,
    cfp.check_formula_id AS from_check_formula_id,
    cfp.indicator_check_id,
    cfp.parameter_pfcbl_category AS to_parameter_pfcbl_category,
    ft.to_family_rsf_pfcbl_id AS to_parameter_rsf_pfcbl_id,
    zids.created_in_reporting_asof_date AS parameter_rsf_pfcbl_id_created_date
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.compute_check_to_parameter_categories cfp ON cfp.for_pfcbl_category = ids.pfcbl_category::text
     JOIN LATERAL ( SELECT (ARRAY[ids.rsf_program_id, ids.rsf_facility_id, ids.rsf_client_id, ids.rsf_borrower_id, ids.rsf_loan_id])[GREATEST(1, COALESCE(cfp.parent_pfcbl_rank, ids.pfcbl_category_rank)::integer)] AS from_rsf_pfcbl_id) fam ON true
     JOIN p_rsf.view_rsf_pfcbl_id_family_tree ft ON ft.from_rsf_pfcbl_id = fam.from_rsf_pfcbl_id AND ft.to_pfcbl_category::text = cfp.parameter_pfcbl_category
     JOIN p_rsf.rsf_pfcbl_ids zids ON zids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id;

-- ----------------------------
-- View structure for compute_check_from_parameter_rsf_pfcbl_id
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."compute_check_from_parameter_rsf_pfcbl_id";
CREATE VIEW "p_rsf"."compute_check_from_parameter_rsf_pfcbl_id" AS  SELECT ft.from_rsf_pfcbl_id AS from_parameter_rsf_pfcbl_id,
    cfp.parameter_indicator_id AS from_parameter_indicator_id,
    ft.to_family_rsf_pfcbl_id AS to_check_rsf_pfcbl_id,
    cfp.check_formula_id AS to_check_formula_id,
    cfp.indicator_check_id,
    cfp.is_calculation_trigger_parameter,
    cfp.parameter_trigger_by_reporting
   FROM p_rsf.view_rsf_pfcbl_id_family_tree ft
     JOIN p_rsf.indicator_check_formula_parameters cfp ON cfp.parameter_pfcbl_category = ft.from_pfcbl_category::text AND cfp.for_pfcbl_category = ft.to_pfcbl_category::text;

-- ----------------------------
-- View structure for view_rsf_setup_review
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_review";
CREATE VIEW "p_rsf"."view_rsf_setup_review" AS  WITH priority_indicators AS (
         SELECT DISTINCT unnest(indf_1.formula_indicator_id_requirements) AS formula_indicator_id
           FROM p_rsf.indicators ind_1
             JOIN p_rsf.indicator_formulas indf_1 ON indf_1.indicator_id = ind_1.indicator_id
          WHERE ind_1.classification IS NOT NULL
        UNION
         SELECT DISTINCT unnest(icf.formula_indicator_ids) AS formula_indicator_id
           FROM p_rsf.indicator_checks ic
             JOIN p_rsf.indicator_check_formulas icf ON icf.indicator_check_id = ic.indicator_check_id
          WHERE ic.check_type = ANY (ARRAY['contract_breach'::text, 'contract_criteria'::text])
        )
 SELECT DISTINCT ON ((sis.is_subscribed = false), sis.data_category_rank, (sis.formula_id IS NOT NULL), sis.indicator_name, sis.rsf_pfcbl_id) sis.rsf_pfcbl_id AS "SYSID",
    sis.indicator_id AS "INDID",
    sn.pfcbl_name,
    sis.indicator_name,
    sis.data_category,
    sis.is_subscribed,
        CASE
            WHEN rdc.data_id IS NULL THEN '{MISSING}'::text
            WHEN ind.indicator_options_group_allows_blanks IS TRUE AND rdc.data_value IS NULL THEN '{NONE}'::text
            WHEN rdc.data_value IS NULL THEN '{BLANK}'::text
            WHEN rdc.data_unit IS NULL THEN rdc.data_value
            ELSE (rdc.data_value || ' '::text) || rdc.data_unit
        END AS data_value,
    rdc.reporting_asof_date AS data_date,
    concat(
        CASE
            WHEN formulas.has_default IS FALSE AND indf.formula_id IS NULL THEN '{Reported: Not calculated}'::text
            ELSE indf.formula_title
        END,
        CASE
            WHEN COALESCE(formulas.num, 0::bigint) > 0 AND COALESCE(dependencies.num, 0::bigint) = 0 THEN ' {No dependencies}'::text
            WHEN COALESCE(formulas.num, 0::bigint) > 0 AND COALESCE(dependencies.num, 0::bigint) > 0 THEN (' {'::text || dependencies.num) || ' formula dependencies}'::text
            ELSE ''::text
        END) AS calculated_using_formula,
    pi.formula_indicator_id IS NOT NULL AS review_priority_indicator,
        CASE
            WHEN formulas.num > 1 OR formulas.has_default IS FALSE THEN true
            ELSE NULL::boolean
        END AS review_formula_setup,
    sis.subscription_comments,
    vai.users_name AS comments_by
   FROM p_rsf.view_rsf_setup_indicator_subscriptions sis
     JOIN p_rsf.indicators ind ON ind.indicator_id = sis.indicator_id
     LEFT JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = sis.rsf_pfcbl_id
     LEFT JOIN p_rsf.view_rsf_pfcbl_id_family_tree ft ON ft.from_rsf_pfcbl_id = sis.rsf_pfcbl_id AND ft.to_pfcbl_category::text = sis.data_category::text AND ft.to_pfcbl_rank <= 3
     LEFT JOIN p_rsf.rsf_data_current rdc ON rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id AND rdc.indicator_id = sis.indicator_id
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.formula_id = sis.formula_id
     LEFT JOIN priority_indicators pi ON pi.formula_indicator_id = sis.indicator_id
     LEFT JOIN p_rsf.view_account_info vai ON vai.account_id = sis.comments_user_id
     LEFT JOIN LATERAL ( SELECT count(form.formula_id) AS num,
            bool_or(form.is_primary_default) AS has_default
           FROM p_rsf.indicator_formulas form
          WHERE form.indicator_id = sis.indicator_id) formulas ON true
     LEFT JOIN LATERAL ( SELECT count(DISTINCT form.indicator_id) AS num
           FROM p_rsf.indicator_formulas form
          WHERE sis.formula_id IS NOT NULL AND form.indicator_id <> sis.indicator_id AND (sis.indicator_id = ANY (form.formula_indicator_id_requirements))) dependencies ON true
  WHERE sis.is_system_indicator IS FALSE AND sis.filter_category_manager IS TRUE
  ORDER BY (sis.is_subscribed = false), sis.data_category_rank, (sis.formula_id IS NOT NULL), sis.indicator_name, sis.rsf_pfcbl_id, (rdc.data_value IS NOT NULL) DESC, rdc.reporting_asof_date DESC;

-- ----------------------------
-- View structure for compute_calculation_from_parameter_rsf_pfcbl_id
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."compute_calculation_from_parameter_rsf_pfcbl_id";
CREATE VIEW "p_rsf"."compute_calculation_from_parameter_rsf_pfcbl_id" AS  SELECT ft.from_rsf_pfcbl_id AS from_parameter_rsf_pfcbl_id,
    ifp.parameter_indicator_id AS from_parameter_indicator_id,
    ifp.indicator_id AS to_calculate_indicator_id,
    ifp.formula_id AS to_calculate_formula_id,
    ft.to_family_rsf_pfcbl_id AS to_calculate_rsf_pfcbl_id,
    ifp.parameter_is_current,
    ifp.parameter_is_previous,
    ifp.parameter_is_all,
    ifp.parameter_is_info,
    ifp.parameter_trigger_by_reporting,
    ifp.parameter_pfcbl_hierarchy
   FROM p_rsf.view_rsf_pfcbl_id_family_tree ft
     JOIN p_rsf.indicator_formula_parameters ifp ON ifp.parameter_pfcbl_category = ft.from_pfcbl_category::text AND ifp.calculate_pfcbl_category = ft.to_pfcbl_category::text;

-- ----------------------------
-- View structure for compute_calculation_to_parameter_rsf_pfcbl_ids
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."compute_calculation_to_parameter_rsf_pfcbl_ids";
CREATE VIEW "p_rsf"."compute_calculation_to_parameter_rsf_pfcbl_ids" AS  SELECT ids.rsf_pfcbl_id AS from_calculate_rsf_pfcbl_id,
    ccp.calculate_formula_id AS from_calculate_formula_id,
    ccp.calculate_indicator_id AS from_calculate_indicator_id,
    ft.to_family_rsf_pfcbl_id AS to_parameter_rsf_pfcbl_id,
    ccp.parameter_pfcbl_category AS to_parameter_pfcbl_category,
    zids.created_in_reporting_asof_date AS parameter_rsf_pfcbl_id_created_date
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.compute_calculation_to_parameter_categories ccp ON ccp.calculate_pfcbl_category = ids.pfcbl_category::text
     JOIN LATERAL ( SELECT (ARRAY[ids.rsf_program_id, ids.rsf_facility_id, ids.rsf_client_id, ids.rsf_borrower_id, ids.rsf_loan_id])[GREATEST(1, COALESCE(ccp.parent_pfcbl_rank, ids.pfcbl_category_rank)::integer)] AS from_rsf_pfcbl_id) fam ON true
     JOIN p_rsf.view_rsf_pfcbl_id_family_tree ft ON ft.from_rsf_pfcbl_id = fam.from_rsf_pfcbl_id AND ft.to_pfcbl_category::text = ccp.parameter_pfcbl_category
     JOIN p_rsf.rsf_pfcbl_ids zids ON zids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id;

-- ----------------------------
-- View structure for error_check_repeated_data_by_row
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."error_check_repeated_data_by_row";
CREATE VIEW "p_rsf"."error_check_repeated_data_by_row" AS  WITH dups AS (
         SELECT ids.rsf_program_id,
            count(*) AS duplicates,
            rd.rsf_pfcbl_id,
            rd.indicator_id,
            rd.data_source_row_id,
            rd.data_value,
            array_agg(DISTINCT rd.reporting_cohort_id) AS cohort_ids,
            array_agg(DISTINCT rd.reporting_asof_date) AS reporting_asof_dates
           FROM p_rsf.rsf_data rd
             JOIN p_rsf.rsf_pfcbl_ids ids ON ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
          WHERE rd.data_source_row_id IS NOT NULL
          GROUP BY ids.rsf_program_id, rd.data_source_row_id, rd.data_value, rd.rsf_pfcbl_id, rd.indicator_id
         HAVING count(*) > 1
          ORDER BY (count(*)) DESC
        )
 SELECT ind.indicator_name,
    ind.data_category,
    dups.rsf_program_id,
    dups.duplicates,
    dups.rsf_pfcbl_id,
    dups.indicator_id,
    dups.data_source_row_id,
    dups.data_value,
    dups.cohort_ids,
    dups.reporting_asof_dates,
    snames.sys_name,
    ind.is_calculated
   FROM dups
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names snames ON snames.rsf_pfcbl_id = dups.rsf_pfcbl_id
     JOIN p_rsf.indicators ind ON ind.indicator_id = dups.indicator_id
  ORDER BY dups.duplicates DESC;

-- ----------------------------
-- View structure for error_check_data_unit_misalignments
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."error_check_data_unit_misalignments";
CREATE VIEW "p_rsf"."error_check_data_unit_misalignments" AS  SELECT sn.sys_name,
    array_to_string(regexp_match(rd.data_submitted, '[[:digit:][:space:]\.,]+([A-Z]+)$'::text), ','::text) AS submitted_data_unit,
    ind.data_unit AS expected_data_unit,
    rd.data_id,
    rd.rsf_pfcbl_id,
    rd.indicator_id,
    rd.reporting_asof_date,
    rd.reporting_cohort_id,
    rd.data_value,
    rd.data_unit,
    rd.data_submitted,
    rd.data_source_row_id
   FROM p_rsf.indicators ind
     JOIN p_rsf.rsf_data rd ON rd.indicator_id = ind.indicator_id
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = rd.rsf_pfcbl_id
  WHERE ind.data_unit IS NOT NULL AND array_to_string(regexp_match(rd.data_submitted, '[[:digit:][:space:]\.,]+([A-Z]+)$'::text), ','::text) IS NOT NULL AND ind.data_unit::text IS DISTINCT FROM array_to_string(regexp_match(rd.data_submitted, '[[:digit:][:space:]\.,]+([A-Z]+)$'::text), ','::text) AND ind.data_unit::text <> 'LCU'::text;

-- ----------------------------
-- View structure for view_reporting_imports_data_checks_current_active
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_reporting_imports_data_checks_current_active";
CREATE VIEW "p_rsf"."view_reporting_imports_data_checks_current_active" AS  SELECT ri.import_id,
    ri.import_rsf_pfcbl_id,
    chk.check_asof_date,
    count(*) AS data_checks_active,
    count(*) FILTER (WHERE ic.check_class::text = 'critical'::text) AS data_checks_critical_active,
    count(*) FILTER (WHERE ic.check_class::text = 'error'::text) AS data_checks_error_active,
    count(*) FILTER (WHERE ic.check_class::text = 'warning'::text) AS data_checks_warning_active,
    count(*) FILTER (WHERE ic.check_class::text = 'info'::text) AS data_checks_info_active
   FROM p_rsf.reporting_imports ri
     JOIN p_rsf.reporting_cohorts rc ON rc.import_id = ri.import_id
     JOIN p_rsf.rsf_data rd ON rd.reporting_cohort_id = rc.reporting_cohort_id
     JOIN p_rsf.rsf_data_current rdc ON rdc.data_id = rd.data_id
     JOIN p_rsf.rsf_data_checks chk ON chk.data_id = rd.data_id AND chk.check_status = 'active'::text
     JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = chk.indicator_check_id
  GROUP BY ri.import_id, ri.import_rsf_pfcbl_id, chk.check_asof_date;

-- ----------------------------
-- View structure for view_reporting_imports_data_counts
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_reporting_imports_data_counts";
CREATE VIEW "p_rsf"."view_reporting_imports_data_counts" AS  SELECT ri.import_id,
    ri.reporting_asof_date,
    ri.import_rsf_pfcbl_id,
    count(rd.data_id) FILTER (WHERE rc.is_reported_cohort IS TRUE) AS data_count_reported,
    count(rd.data_id) FILTER (WHERE rc.is_calculated_cohort IS TRUE) AS data_count_calculated,
    count(rdc.data_id) FILTER (WHERE rc.is_reported_cohort IS TRUE) AS data_current_count_reported,
    count(rdc.data_id) FILTER (WHERE rc.is_calculated_cohort IS TRUE) AS data_current_count_calculated
   FROM p_rsf.reporting_imports ri
     JOIN p_rsf.reporting_cohorts rc ON rc.import_id = ri.import_id
     LEFT JOIN p_rsf.rsf_data rd ON rd.reporting_cohort_id = rc.reporting_cohort_id
     LEFT JOIN p_rsf.rsf_data_current rdc ON rdc.data_id = rd.data_id
  GROUP BY ri.import_id, ri.reporting_asof_date, ri.import_rsf_pfcbl_id;

-- ----------------------------
-- View structure for util_index_analyis
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."util_index_analyis";
CREATE VIEW "p_rsf"."util_index_analyis" AS  SELECT idstat.relname AS table_name,
    idstat.indexrelname AS index_name,
    idstat.idx_scan AS index_scans_count,
    pg_size_pretty(pg_relation_size(idstat.indexrelid::regclass)) AS index_size,
    pg_relation_size(idstat.indexrelid::regclass) AS index_size_bytes,
    tabstat.idx_scan AS table_reads_index_count,
    idstat.idx_scan::numeric * 100.0 / NULLIF(tabstat.idx_scan, 0)::numeric AS table_reads_index_scans_pct,
    tabstat.seq_scan AS table_reads_seq_count,
    tabstat.seq_scan + tabstat.idx_scan AS table_reads_count,
    tabstat.n_tup_upd + tabstat.n_tup_ins + tabstat.n_tup_del AS table_writes_count,
    pg_size_pretty(pg_relation_size(idstat.relid::regclass)) AS table_size,
    pg_indexes.indexdef
   FROM pg_stat_user_indexes idstat
     JOIN pg_indexes ON idstat.indexrelname = pg_indexes.indexname AND idstat.schemaname = pg_indexes.schemaname
     JOIN pg_stat_user_tables tabstat ON idstat.relid = tabstat.relid
  ORDER BY idstat.idx_scan DESC, (pg_relation_size(idstat.indexrelid::regclass)) DESC;

-- ----------------------------
-- View structure for util_reporting_cohort_info_log_times
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."util_reporting_cohort_info_log_times";
CREATE VIEW "p_rsf"."util_reporting_cohort_info_log_times" AS  SELECT reporting_cohort_info.reporting_cohort_id,
    reporting_cohort_info.upload_filename,
    stimes.st AS statement_time,
    ttimes.tt AS total_time,
    "substring"(log.log, '([A-Za-z_]+)'::text) AS func,
    log.log
   FROM p_rsf.reporting_cohort_info
     LEFT JOIN LATERAL unnest(string_to_array(reporting_cohort_info.metadata ->> 'log'::text, '\n'::text)) log(log) ON true
     LEFT JOIN LATERAL ( SELECT (regexp_match(log.log, 'Done!\s?@?(\d+\.?\d*).+'::text))[1]::numeric AS st) stimes ON true
     LEFT JOIN LATERAL ( SELECT (regexp_match(log.log, 'running total @(\d+\.?\d*)'::text))[1]::numeric AS tt) ttimes ON true
  WHERE log.log IS NOT NULL;

-- ----------------------------
-- View structure for compute_calculation_grouping
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."compute_calculation_grouping";
CREATE VIEW "p_rsf"."compute_calculation_grouping" AS  SELECT calculate_formula_id,
    calculate_indicator_id,
    array_to_string(array_agg(DISTINCT computation_code ORDER BY computation_code), '-'::text) AS computation_code,
    bit_or(
        CASE
            WHEN computation_code = 'L'::text THEN 2::double precision ^ 0::double precision
            WHEN computation_code = 'B'::text THEN 2::double precision ^ 1::double precision
            WHEN computation_code = ANY (ARRAY['C'::text, 'F'::text, 'F>C'::text, 'C>F'::text]) THEN 2::double precision ^ 2::double precision
            WHEN computation_code = 'P'::text THEN 2::double precision ^ 2::double precision
            WHEN computation_code = 'B>L'::text THEN (((2::double precision ^ 4::double precision)::integer | (2::double precision ^ 1::double precision)::integer) | (2::double precision ^ 0::double precision)::integer)::double precision
            WHEN computation_code = ANY (ARRAY['C>L'::text, 'F>L'::text]) THEN (((2::double precision ^ 5::double precision)::integer | (2::double precision ^ 0::double precision)::integer) | (2::double precision ^ 2::double precision)::integer)::double precision
            WHEN computation_code = 'P>L'::text THEN (((2::double precision ^ 6::double precision)::integer | (2::double precision ^ 0::double precision)::integer) | (2::double precision ^ 3::double precision)::integer)::double precision
            WHEN computation_code = ANY (ARRAY['C>B'::text, 'F>B'::text]) THEN (((2::double precision ^ 7::double precision)::integer | (2::double precision ^ 2::double precision)::integer) | (2::double precision ^ 1::double precision)::integer)::double precision
            WHEN computation_code = 'P>B'::text THEN (((2::double precision ^ 8::double precision)::integer | (2::double precision ^ 3::double precision)::integer) | (2::double precision ^ 1::double precision)::integer)::double precision
            WHEN computation_code = ANY (ARRAY['P>C'::text, 'P>F'::text]) THEN (((2::double precision ^ 9::double precision)::integer | (2::double precision ^ 2::double precision)::integer) | (2::double precision ^ 3::double precision)::integer)::double precision
            ELSE 0::double precision
        END::integer) AS computation_group
   FROM p_rsf.compute_calculation_to_parameter_categories ccp
  GROUP BY calculate_indicator_id, calculate_formula_id;

-- ----------------------------
-- View structure for view_account_info
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_account_info";
CREATE VIEW "p_rsf"."view_account_info" AS  SELECT account_id,
    users_name,
    login_email AS users_login,
    is_system_account
   FROM p_rsf.dblink_account_info() dblink_account_info(account_id, users_name, login_email, is_system_account);

-- ----------------------------
-- View structure for view_rsf_setup_programs_template_actions
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_programs_template_actions";
CREATE VIEW "p_rsf"."view_rsf_setup_programs_template_actions" AS  SELECT ids.rsf_program_id,
    fth.rsf_pfcbl_id,
    fth.template_id,
    fth.header_id,
    sn.sys_name,
    rt.template_name,
    fth.template_header_sheet_name,
    fth.template_header,
    fth.template_header_encounter_index,
    fth.action,
    fth.action_mapping AS remap_header
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.rsf_program_facility_template_headers fth ON fth.rsf_pfcbl_id = ids.rsf_pfcbl_id
     JOIN p_rsf.reporting_templates rt ON rt.template_id = fth.template_id
     JOIN LATERAL ( SELECT nai.sys_name
           FROM p_rsf.rsf_data_current_names_and_ids nai
          WHERE nai.rsf_pfcbl_id = ids.rsf_pfcbl_id
          ORDER BY nai.reporting_asof_date
         LIMIT 1) sn ON true
  ORDER BY ids.rsf_program_id, sn.sys_name, rt.template_name, fth.template_header_sheet_name, fth.template_header, fth.template_header_encounter_index, fth.action, fth.action_mapping;

-- ----------------------------
-- View structure for view_rsf_pfcbl_id_family_tree
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pfcbl_id_family_tree";
CREATE VIEW "p_rsf"."view_rsf_pfcbl_id_family_tree" AS  SELECT unnest((ARRAY[0, ids.rsf_program_id, ids.rsf_facility_id, ids.rsf_client_id, ids.rsf_borrower_id, ids.rsf_loan_id])[1:ids.pfcbl_category_rank]) AS to_family_rsf_pfcbl_id,
    unnest((ARRAY['global'::text, 'program'::text, 'facility'::text, 'client'::text, 'borrower'::text, 'loan'::text])[1:ids.pfcbl_category_rank])::character varying AS to_pfcbl_category,
    unnest((ARRAY[0, 1, 2, 3, 4, 5])[1:ids.pfcbl_category_rank])::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_program_id AS from_rsf_pfcbl_id,
    'program'::character varying(255) AS from_pfcbl_category,
    1::smallint AS from_pfcbl_rank,
    'child'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 1
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_facility_id AS from_rsf_pfcbl_id,
    'facility'::character varying(255) AS from_pfcbl_category,
    2::smallint AS from_pfcbl_rank,
    'child'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 2
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_client_id AS from_rsf_pfcbl_id,
    'client'::character varying(255) AS from_pfcbl_category,
    3::smallint AS from_pfcbl_rank,
    'child'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 3
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_borrower_id AS from_rsf_pfcbl_id,
    'borrower'::character varying(255) AS from_pfcbl_category,
    4::smallint AS from_pfcbl_rank,
    'child'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 4
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'self'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids;

-- ----------------------------
-- View structure for view_rsf_setup_programs_settings
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_programs_settings";
CREATE VIEW "p_rsf"."view_rsf_setup_programs_settings" AS  SELECT rps.rsf_program_id,
    ids.rsf_pfcbl_id,
    ids.pfcbl_category,
    sn.sys_name,
    rps.setting_name,
    rps.setting_value
   FROM p_rsf.rsf_program_settings rps
     JOIN p_rsf.rsf_pfcbl_ids ids ON ids.rsf_program_id = rps.rsf_program_id
     JOIN LATERAL ( SELECT nai.sys_name
           FROM p_rsf.rsf_data_current_names_and_ids nai
          WHERE nai.rsf_pfcbl_id = ids.rsf_pfcbl_id
          ORDER BY nai.reporting_asof_date
         LIMIT 1) sn ON true
  WHERE ids.pfcbl_category_rank <= 1;

-- ----------------------------
-- View structure for view_rsf_setup_programs_guidance
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_programs_guidance";
CREATE VIEW "p_rsf"."view_rsf_setup_programs_guidance" AS  SELECT ids.rsf_program_id,
    icg.indicator_check_guidance_id,
    icg.indicator_check_id,
    icg.for_indicator_id,
    icg.for_pfcbl_category,
    sn.sys_name,
    ind.indicator_name,
    ic.check_name,
    icg.guidance,
    icg.is_resolving_guidance,
    icg.overwrite_check_class,
    icg.user_id AS created_by_user_id,
    fcg.rsf_pfcbl_id,
    fcg.applied_by_user_id
   FROM p_rsf.indicator_check_guidance icg
     JOIN p_rsf.rsf_program_facility_check_guidance fcg ON fcg.indicator_check_guidance_id = icg.indicator_check_guidance_id
     JOIN p_rsf.rsf_pfcbl_ids ids ON ids.rsf_pfcbl_id = fcg.rsf_pfcbl_id
     JOIN p_rsf.indicators ind ON ind.indicator_id = icg.for_indicator_id
     JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = icg.indicator_check_id
     JOIN p_rsf.rsf_pfcbl_categories rpc ON rpc.pfcbl_category::text = ind.data_category::text
     JOIN LATERAL ( SELECT nai.sys_name
           FROM p_rsf.rsf_data_current_names_and_ids nai
          WHERE nai.rsf_pfcbl_id = ids.rsf_pfcbl_id
          ORDER BY nai.reporting_asof_date
         LIMIT 1) sn ON true
  WHERE (EXISTS ( SELECT fcg_1.rsf_pfcbl_id,
            fcg_1.indicator_check_guidance_id,
            fcg_1.rsf_program_id,
            fcg_1.rsf_facility_id,
            fcg_1.applied_by_user_id,
            fcg_1.application_time
           FROM p_rsf.rsf_program_facility_check_guidance fcg_1
          WHERE fcg_1.indicator_check_guidance_id = icg.indicator_check_guidance_id))
  ORDER BY rpc.pfcbl_rank, (icg.for_pfcbl_category = 'program'::text) DESC, ind.indicator_name, ic.check_name;

-- ----------------------------
-- View structure for view_rsf_pfcbl_id_current_sys_names
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pfcbl_id_current_sys_names";
CREATE VIEW "p_rsf"."view_rsf_pfcbl_id_current_sys_names" AS  SELECT DISTINCT ON (ids.rsf_pfcbl_id) ids.rsf_pfcbl_id,
    ids.pfcbl_category,
    nids.sys_name,
    ids.rsf_program_id,
    nids.pfcbl_name
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.rsf_data_current_names_and_ids nids ON nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
  ORDER BY ids.rsf_pfcbl_id, nids.reporting_asof_date DESC;

-- ----------------------------
-- View structure for error_check_rsf_data_calculation_evaluations_category_alignment
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."error_check_rsf_data_calculation_evaluations_category_alignment";
CREATE VIEW "p_rsf"."error_check_rsf_data_calculation_evaluations_category_alignment" AS  SELECT ids.rsf_pfcbl_id,
    ids.pfcbl_category,
    ind.data_category,
    ind.indicator_name
   FROM p_rsf.rsf_data_calculation_evaluations dce
     JOIN p_rsf.rsf_pfcbl_ids ids ON ids.rsf_pfcbl_id = dce.rsf_pfcbl_id
     JOIN p_rsf.indicators ind ON ind.indicator_id = dce.indicator_id
  WHERE ids.pfcbl_category::text <> ind.data_category::text;

-- ----------------------------
-- View structure for view_rsf_program_facility_template_headers
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_program_facility_template_headers";
CREATE VIEW "p_rsf"."view_rsf_program_facility_template_headers" AS  SELECT sn.rsf_pfcbl_id,
    rt.template_id,
    sn.sys_name AS "SYSNAME",
    rt.template_name,
    fth.header_id,
    fth.template_header_sheet_name,
    fth.template_header,
    fth.action,
    fth.comment,
    fth.map_indicator_id,
    ind.indicator_name,
    fth.map_formula_id,
    (find.indicator_name::text || ':'::text) || indf.formula_title AS calculation_formula,
    fth.map_check_formula_id,
    (ic.check_name::text || ':'::text) || icf.check_formula_title AS check_formula,
    fth.template_header_full_normalized
   FROM p_rsf.rsf_program_facility_template_headers fth
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = fth.rsf_pfcbl_id
     JOIN p_rsf.reporting_templates rt ON rt.template_id = fth.template_id
     LEFT JOIN p_rsf.indicators ind ON ind.indicator_id = fth.map_indicator_id
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.formula_id = fth.map_formula_id
     LEFT JOIN p_rsf.indicators find ON find.indicator_id = indf.indicator_id
     LEFT JOIN p_rsf.indicator_check_formulas icf ON icf.check_formula_id = fth.map_check_formula_id
     LEFT JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = icf.indicator_check_id
  ORDER BY sn.rsf_pfcbl_id, fth.template_header_sheet_name, fth.template_header, fth.action;

-- ----------------------------
-- View structure for view_rsf_pfcbl_currency_units_asof_date
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pfcbl_currency_units_asof_date";
CREATE VIEW "p_rsf"."view_rsf_pfcbl_currency_units_asof_date" AS  SELECT ids.rsf_program_id,
    ids.rsf_pfcbl_id AS for_rsf_pfcbl_id,
    rdc.data_id AS lcu_unit_data_id,
        CASE
            WHEN rdc.reporting_asof_date < ids.created_in_reporting_asof_date THEN ids.created_in_reporting_asof_date
            ELSE rdc.reporting_asof_date
        END AS reporting_asof_date,
    rdc.data_value AS data_unit_value,
    parents.pfcbl_category_rank AS data_id_pfcbl_rank,
    false AS is_defined_lcu
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN LATERAL unnest(ARRAY[ids.rsf_program_id, ids.rsf_facility_id, ids.rsf_client_id, ids.rsf_borrower_id, ids.rsf_loan_id]) parent_rsf_pfcbl_id(parent_rsf_pfcbl_id) ON true
     JOIN p_rsf.rsf_pfcbl_ids parents ON parents.rsf_pfcbl_id = parent_rsf_pfcbl_id.parent_rsf_pfcbl_id
     JOIN p_rsf.indicators ind ON ind.data_category::text = parents.pfcbl_category::text AND ind.indicator_sys_category::text = 'entity_local_currency_unit'::text
     JOIN p_rsf.rsf_data_current rdc ON rdc.rsf_pfcbl_id = parents.rsf_pfcbl_id AND rdc.indicator_id = ind.indicator_id
  WHERE rdc.data_value IS NOT NULL AND rdc.data_value <> 'LCU'::text AND NOT (EXISTS ( SELECT dd.data_id,
            dd.rsf_pfcbl_id,
            dd.indicator_id,
            dd.reporting_asof_date,
            dd.data_value,
            dd.data_unit,
            dd.data_unit_data_id,
            ind_1.indicator_id,
            ind_1.indicator_name,
            ind_1.indicator_sys_category,
            ind_1.data_category,
            ind_1.data_type,
            ind_1.data_unit,
            ind_1.default_value,
            ind_1.definition,
            ind_1.label_id,
            ind_1.indicator_options_group_id,
            ind_1.indicator_options_group_allows_blanks,
            ind_1.indicator_options_group_allows_multiples,
            ind_1.is_calculated,
            ind_1.is_system,
            ind_1.is_data_unit,
            ind_1.is_system_calculated,
            ind_1.is_setup,
            ind_1.modification_time,
            ind_1.version_number,
            ind_1.is_static_nonreporting,
            ind_1.default_subscription,
            ind_1.is_periodic_or_flow_reporting,
            ind_1.classification,
            ind_1.sort_preference,
            ind_1.created_by_user_id,
            ind_1.modified_by_user_id,
            ind_1.pfcbl_rank
           FROM p_rsf.rsf_data_current dd
             JOIN p_rsf.indicators ind_1 ON ind_1.indicator_id = dd.indicator_id
          WHERE dd.rsf_pfcbl_id = ids.rsf_pfcbl_id AND dd.indicator_id = ind_1.indicator_id AND dd.reporting_asof_date <= rdc.reporting_asof_date AND ind_1.indicator_sys_category::text = 'entity_currency_unit'::text AND dd.data_value IS NOT NULL AND dd.data_value <> 'LCU'::text))
UNION ALL
 SELECT ids.rsf_program_id,
    ids.rsf_pfcbl_id AS for_rsf_pfcbl_id,
    rdc.data_id AS lcu_unit_data_id,
    rdc.reporting_asof_date,
    rdc.data_value AS data_unit_value,
    ids.pfcbl_category_rank AS data_id_pfcbl_rank,
    true AS is_defined_lcu
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.indicators ind ON ind.data_category::text = ids.pfcbl_category::text
     JOIN p_rsf.rsf_data_current rdc ON rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id AND rdc.indicator_id = ind.indicator_id
  WHERE ind.indicator_sys_category::text = 'entity_currency_unit'::text AND rdc.data_value IS NOT NULL AND rdc.data_value <> 'LCU'::text;

-- ----------------------------
-- View structure for util_reporting_cohort_info_process_times
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."util_reporting_cohort_info_process_times";
CREATE VIEW "p_rsf"."util_reporting_cohort_info_process_times" AS  SELECT rc."!dep-rsf_program_id" AS rsf_program_id,
    sn.sys_name,
    rc.reporting_asof_date,
    rc.reporting_rsf_pfcbl_id,
    rci.reporting_cohort_id,
    ((rci.metadata -> 'timing'::text) ->> 'parse_time'::text)::numeric AS parse_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'process_time'::text)::numeric AS process_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'upload_time'::text)::numeric AS upload_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'total_time'::text)::numeric AS total_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'backup_time'::text)::numeric AS backup_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'calculate_time'::text)::numeric AS calculate_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'check_time'::text)::numeric AS check_time_sec
   FROM p_rsf.reporting_cohort_info rci
     JOIN p_rsf.reporting_cohorts rc ON rc.reporting_cohort_id = rci.reporting_cohort_id
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
  WHERE rc."!dep-parent_reporting_cohort_id" IS NULL;

-- ----------------------------
-- View structure for view_rsf_program_settings
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_program_settings";
CREATE VIEW "p_rsf"."view_rsf_program_settings" AS  SELECT rp.rsf_program_id,
    ps.setting_name,
    COALESCE(rps.setting_value, ps.default_value) AS setting_value,
    ps.default_data_type,
    ps.setting_group
   FROM p_rsf.rsf_programs rp
     CROSS JOIN p_rsf.program_settings ps
     LEFT JOIN p_rsf.rsf_program_settings rps ON rps.rsf_program_id = rp.rsf_program_id AND rps.setting_name = ps.setting_name;

-- ----------------------------
-- View structure for view_rsf_setup_facility_terms_missing
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_facility_terms_missing";
CREATE VIEW "p_rsf"."view_rsf_setup_facility_terms_missing" AS  SELECT rsf_facility_id,
    sys_name,
    metric,
    metric_id,
    metric_name,
    formula_id,
    formula_title,
    parameter_name,
    parameter_id,
    parameter_ids,
    data_value,
    data_unit,
    reporting_asof_date,
    data_id IS NULL AS is_unreported
   FROM ( SELECT ids.rsf_facility_id,
            sn.sys_name,
            'indicator'::text AS metric,
            ind.indicator_id AS metric_id,
            ind.indicator_name AS metric_name,
            sis.formula_id,
            indf.formula_title,
            indf.formula_indicator_id_requirements AS parameter_ids,
            pind.indicator_name AS parameter_name,
            pind.indicator_id AS parameter_id,
            rdc.data_value,
            rdc.data_unit,
            rdc.reporting_asof_date,
            rdc.data_id
           FROM p_rsf.rsf_pfcbl_ids ids
             JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
             JOIN p_rsf.view_rsf_setup_indicator_subscriptions sis ON sis.rsf_pfcbl_id = ids.rsf_pfcbl_id
             JOIN p_rsf.indicators ind ON ind.indicator_id = sis.indicator_id
             JOIN p_rsf.indicator_formulas indf ON indf.formula_id = sis.formula_id
             JOIN p_rsf.indicator_formula_parameters ifp ON ifp.formula_id = sis.formula_id AND ifp.parameter_pfcbl_category = 'facility'::text
             JOIN p_rsf.indicators pind ON pind.indicator_id = ifp.parameter_indicator_id
             LEFT JOIN p_rsf.view_rsf_setup_indicator_subscriptions psis ON psis.rsf_pfcbl_id = ids.rsf_pfcbl_id AND psis.indicator_id = pind.indicator_id
             LEFT JOIN p_rsf.rsf_data_current rdc ON rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id AND rdc.indicator_id = psis.indicator_id
             LEFT JOIN p_rsf.rsf_data rd ON rd.data_id = rdc.data_id
          WHERE ids.pfcbl_category::text = 'facility'::text AND sis.formula_id IS NOT NULL AND psis.formula_id IS NULL AND rdc.data_value IS NULL AND rd.data_submitted IS NULL AND (sis.is_subscribed IS TRUE OR sis.is_auto_subscribed IS TRUE)
        UNION ALL
         SELECT ids.rsf_facility_id,
            sn.sys_name,
            'check'::text AS metric,
            ic.indicator_check_id AS metric_id,
            ic.check_name AS metric_name,
            scs.check_formula_id AS formula_id,
            icf.check_formula_title AS formula_title,
            icf.formula_indicator_ids AS parameter_ids,
            pind.indicator_name AS parameter_name,
            pind.indicator_id AS parameter_id,
            rdc.data_value,
            rdc.data_unit,
            rdc.reporting_asof_date,
            rdc.data_id
           FROM p_rsf.rsf_pfcbl_ids ids
             JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
             JOIN p_rsf.view_rsf_setup_check_subscriptions scs ON scs.rsf_pfcbl_id = ids.rsf_pfcbl_id
             JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = scs.indicator_check_id
             JOIN p_rsf.indicator_check_formulas icf ON icf.check_formula_id = scs.check_formula_id
             JOIN p_rsf.indicator_check_formula_parameters cfp ON cfp.check_formula_id = scs.check_formula_id AND cfp.parameter_pfcbl_category = 'facility'::text
             JOIN p_rsf.indicators pind ON pind.indicator_id = cfp.parameter_indicator_id
             LEFT JOIN p_rsf.view_rsf_setup_indicator_subscriptions psis ON psis.rsf_pfcbl_id = ids.rsf_pfcbl_id AND psis.indicator_id = pind.indicator_id
             LEFT JOIN p_rsf.rsf_data_current rdc ON rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id AND rdc.indicator_id = psis.indicator_id
             LEFT JOIN p_rsf.rsf_data rd ON rd.data_id = rdc.data_id
          WHERE ids.pfcbl_category::text = 'facility'::text AND psis.formula_id IS NULL AND rdc.data_value IS NULL AND rd.data_submitted IS NULL AND (scs.is_subscribed IS TRUE OR scs.is_auto_subscribed IS TRUE)) terms;

-- ----------------------------
-- View structure for view_rsf_setup_check_subscriptions
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_check_subscriptions";
CREATE VIEW "p_rsf"."view_rsf_setup_check_subscriptions" AS  SELECT ids.rsf_pfcbl_id,
    ids.pfcbl_category,
    pfc.check_formula_id,
    pfc.indicator_check_id,
    ids.rsf_program_id,
    ids.rsf_facility_id,
    pfc.is_subscribed,
    pfc.is_subscribed IS FALSE AS is_unsubscribed,
    pfc.is_auto_subscribed,
    icf.check_pfcbl_rank,
    icf.check_pfcbl_category,
    pfc.subscription_comments,
    pfc.comments_user_id,
    ids.pfcbl_category_rank = icf.check_pfcbl_rank AS filter_matched_pfcbl_indicators,
    ids.pfcbl_category_rank = icf.check_pfcbl_rank AND ids.pfcbl_category_rank <= 2 OR ids.pfcbl_category_rank = 2 AND icf.check_pfcbl_rank >= 2 AS filter_category_manager,
        CASE
            WHEN icf.check_pfcbl_rank = 0 THEN 0
            WHEN icf.check_pfcbl_rank = 1 THEN ids.rsf_program_id
            ELSE ids.rsf_facility_id
        END AS category_manager_rsf_pfcbl_id
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.rsf_setup_checks pfc ON pfc.rsf_pfcbl_id = ids.rsf_facility_id OR pfc.rsf_pfcbl_id = ids.rsf_program_id OR pfc.rsf_pfcbl_id = 0
     JOIN p_rsf.indicator_check_formulas icf ON icf.check_formula_id = pfc.check_formula_id;

-- ----------------------------
-- View structure for view_rsf_setup_check_monitoring
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_check_monitoring";
CREATE VIEW "p_rsf"."view_rsf_setup_check_monitoring" AS  SELECT sis.rsf_pfcbl_id,
    sis.pfcbl_category,
    sis.filter_matched_pfcbl_indicators,
    cfp.indicator_check_id,
    cfp.check_formula_id,
    ic.check_class,
    ic.check_type,
    ic.check_name,
    ic.check_pfcbl_category,
    bool_or(sis.is_unsubscribed IS TRUE) FILTER (WHERE cfp.is_calculation_trigger_parameter IS TRUE) AS is_not_monitorable,
    bool_and(sis.is_subscribed IS TRUE) FILTER (WHERE cfp.is_calculation_trigger_parameter IS TRUE) AS is_calculable,
    ic.auto_subscribe IS TRUE AND bool_and(sis.is_subscribed IS TRUE) AS is_auto_monitorable,
    array_agg(cfp.parameter_indicator_id ORDER BY cfp.parameter_indicator_id) FILTER (WHERE sis.is_subscribed IS FALSE) AS unmonitored_parameter_ids,
    NOT sis.pfcbl_category::text IS DISTINCT FROM ic.check_pfcbl_category OR sis.pfcbl_category::text = 'facility'::text AND (ic.check_pfcbl_category = ANY (ARRAY['client'::text, 'borrower'::text, 'loan'::text])) AS filter_category_manager
   FROM p_rsf.view_rsf_setup_indicator_subscriptions sis
     JOIN p_rsf.indicator_check_formula_parameters cfp ON cfp.parameter_indicator_id = sis.indicator_id
     JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = cfp.indicator_check_id
  GROUP BY sis.rsf_pfcbl_id, sis.pfcbl_category, sis.filter_matched_pfcbl_indicators, cfp.indicator_check_id, cfp.check_formula_id, ic.check_class, ic.check_type, ic.check_name, ic.check_pfcbl_category, ic.auto_subscribe;

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."export_template_reports_export_template_report_id_seq"
OWNED BY "p_rsf"."export_template_reports"."export_template_report_id";
SELECT setval('"p_rsf"."export_template_reports_export_template_report_id_seq"', 2, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."export_templates_export_template_id_seq"
OWNED BY "p_rsf"."export_templates"."export_template_id";
SELECT setval('"p_rsf"."export_templates_export_template_id_seq"', 1, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."exporting_cohorts_exporting_cohort_id_seq"', 610, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."import_templates_import_id_seq"
OWNED BY "p_rsf"."reporting_imports"."import_id";
SELECT setval('"p_rsf"."import_templates_import_id_seq"', 101833, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_check_formulas_check_formula_id_seq"
OWNED BY "p_rsf"."indicator_check_formulas"."check_formula_id";
SELECT setval('"p_rsf"."indicator_check_formulas_check_formula_id_seq"', 228, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_check_guidance_guidance_id_seq"
OWNED BY "p_rsf"."indicator_check_guidance"."indicator_check_guidance_id";
SELECT setval('"p_rsf"."indicator_check_guidance_guidance_id_seq"', 141, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_checks_check_id_seq"
OWNED BY "p_rsf"."indicator_checks"."indicator_check_id";
SELECT setval('"p_rsf"."indicator_checks_check_id_seq"', 47991, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_formulas_formula_id_seq"
OWNED BY "p_rsf"."indicator_formulas"."formula_id";
SELECT setval('"p_rsf"."indicator_formulas_formula_id_seq"', 508, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_object_logs_log_id_seq"
OWNED BY "p_rsf"."indicator_object_logs"."log_id";
SELECT setval('"p_rsf"."indicator_object_logs_log_id_seq"', 1, false);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_option_groups_option_group_id_seq"
OWNED BY "p_rsf"."indicator_options_groups"."options_group_id";
SELECT setval('"p_rsf"."indicator_option_groups_option_group_id_seq"', 39, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicators_indicator_id_seq"
OWNED BY "p_rsf"."indicators"."indicator_id";
SELECT setval('"p_rsf"."indicators_indicator_id_seq"', 157840, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."label_ids_label_id_seq"
OWNED BY "p_rsf"."label_ids"."label_id";
SELECT setval('"p_rsf"."label_ids_label_id_seq"', 2680, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."labels_label_id_seq"
OWNED BY "p_rsf"."labels"."label_id";
SELECT setval('"p_rsf"."labels_label_id_seq"', 1, false);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."reporting_templates_template_id_seq"
OWNED BY "p_rsf"."reporting_templates"."template_id";
SELECT setval('"p_rsf"."reporting_templates_template_id_seq"', 12, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."reports_report_id_seq"
OWNED BY "p_rsf"."reports"."report_id";
SELECT setval('"p_rsf"."reports_report_id_seq"', 76, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_data_calculation_profiles_calculation_profile_id_seq"', 5170327, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_data_checks_evaluation_id_seq"', 4123129, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_data_cohort_sequence"', 103618, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_data_data_id_seq"', 26178702, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_pfcbl_ids_rsf_pfcbl_id_seq"', 571930, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."rsf_program_facility_template_headers_header_id_seq"
OWNED BY "p_rsf"."rsf_program_facility_template_headers"."header_id";
SELECT setval('"p_rsf"."rsf_program_facility_template_headers_header_id_seq"', 980, true);

-- ----------------------------
-- Indexes structure for table !dep-rsf_pfcbl_id_family
-- ----------------------------
CREATE INDEX "family_tree_children" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "parent_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE child_pfcbl_rank > parent_pfcbl_rank;
CREATE INDEX "family_tree_parents" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "child_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE parent_pfcbl_rank <= child_pfcbl_rank;
CREATE UNIQUE INDEX "rsf_pfcbl_id_family-global_is_unique_parent" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "parent_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE parent_pfcbl_category::text = 'global'::text;
CREATE INDEX "rsf_pfcbl_id_family_cascade_down_idx" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "parent_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "child_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
COMMENT ON INDEX "p_rsf"."rsf_pfcbl_id_family_cascade_down_idx" IS 'Each parent entity (may) have many child entities';
CREATE UNIQUE INDEX "rsf_pfcbl_id_family_cascade_up_idx" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "child_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parent_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
COMMENT ON INDEX "p_rsf"."rsf_pfcbl_id_family_cascade_up_idx" IS 'Each child entity only has one parent entity';
CREATE INDEX "rsf_pfcbl_id_family_child_pfcbl_category_idx" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "child_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_id_family_child_rsf_pfcbl_id_idx" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "child_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_id_family_parent_pfcbl_category_idx" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "parent_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_id_family_parent_rsf_pfcbl_id_idx" ON "p_rsf"."!dep-rsf_pfcbl_id_family" USING btree (
  "parent_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."!dep-rsf_pfcbl_id_family" CLUSTER ON "rsf_pfcbl_id_family_parent_rsf_pfcbl_id_idx";

-- ----------------------------
-- Primary Key structure for table !dep-rsf_pfcbl_id_family
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_pfcbl_id_family" ADD CONSTRAINT "rsf_pfcbl_id_family_pkey" PRIMARY KEY ("parent_rsf_pfcbl_id", "child_rsf_pfcbl_id");

-- ----------------------------
-- Cluster option for table !dep-rsf_pfcbl_id_family
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_pfcbl_id_family" CLUSTER ON "rsf_pfcbl_id_family_parent_rsf_pfcbl_id_idx";

-- ----------------------------
-- Indexes structure for table !dep-rsf_program_reporting_dates
-- ----------------------------
CREATE UNIQUE INDEX "rsf_program_reporting_dates_rsf_program_id_reporting_sequen_idx" ON "p_rsf"."!dep-rsf_program_reporting_dates" USING btree (
  "rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_sequence_rank" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table !dep-rsf_program_reporting_dates
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_program_reporting_dates" ADD CONSTRAINT "rsf_program_reporting_dates_pkey" PRIMARY KEY ("rsf_program_id", "valid_reporting_date");

-- ----------------------------
-- Primary Key structure for table export_template_reports
-- ----------------------------
ALTER TABLE "p_rsf"."export_template_reports" ADD CONSTRAINT "export_template_reports_pkey" PRIMARY KEY ("export_template_report_id");

-- ----------------------------
-- Primary Key structure for table export_templates
-- ----------------------------
ALTER TABLE "p_rsf"."export_templates" ADD CONSTRAINT "export_templates_pkey" PRIMARY KEY ("export_template_id");

-- ----------------------------
-- Indexes structure for table exporting_cohorts
-- ----------------------------
CREATE UNIQUE INDEX "exporting_cohorts_reporting_key_idx" ON "p_rsf"."exporting_cohorts" USING btree (
  "reporting_key" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table exporting_cohorts
-- ----------------------------
CREATE TRIGGER "trigger_set_exporting_cohorts_reporting_key" BEFORE INSERT OR UPDATE ON "p_rsf"."exporting_cohorts"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_exporting_cohorts_reporting_key"();

-- ----------------------------
-- Primary Key structure for table exporting_cohorts
-- ----------------------------
ALTER TABLE "p_rsf"."exporting_cohorts" ADD CONSTRAINT "exporting_cohorts_pkey" PRIMARY KEY ("exporting_cohort_id");

-- ----------------------------
-- Indexes structure for table indicator_check_formula_parameters
-- ----------------------------
CREATE INDEX "indicator_check_formula_param_check_formula_id_for_pfcbl_ca_idx" ON "p_rsf"."indicator_check_formula_parameters" USING btree (
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "for_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_check_formula_param_indicator_check_id_for_indica_idx" ON "p_rsf"."indicator_check_formula_parameters" USING btree (
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parameter_pfcbl_hierarchy" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_check_formula_param_indicator_check_id_parameter__idx" ON "p_rsf"."indicator_check_formula_parameters" USING btree (
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parameter_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parameter_pfcbl_hierarchy" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_check_formula_param_parameter_indicator_id_parame_idx" ON "p_rsf"."indicator_check_formula_parameters" USING btree (
  "parameter_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parameter_pfcbl_hierarchy" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_check_formula_parameters_parameter_indicator_id_idx" ON "p_rsf"."indicator_check_formula_parameters" USING btree (
  "parameter_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Checks structure for table indicator_check_formula_parameters
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_formula_parameters" ADD CONSTRAINT "parameter_pfcbl_hierarchy_valid_values" CHECK (parameter_pfcbl_hierarchy = ANY (ARRAY['parent'::text, 'self'::text, 'child'::text]));

-- ----------------------------
-- Primary Key structure for table indicator_check_formula_parameters
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_formula_parameters" ADD CONSTRAINT "indicator_check_formula_parameters_pkey" PRIMARY KEY ("check_formula_id", "parameter_indicator_id");

-- ----------------------------
-- Indexes structure for table indicator_check_formulas
-- ----------------------------
CREATE UNIQUE INDEX "indicator_check_formulas_check_formula_id_check_pfcbl_categ_idx" ON "p_rsf"."indicator_check_formulas" USING btree (
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_check_formulas_check_pfcbl_category_idx" ON "p_rsf"."indicator_check_formulas" USING btree (
  "check_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_check_formulas_check_pfcbl_category_parent_groupi_idx" ON "p_rsf"."indicator_check_formulas" USING btree (
  "check_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "parent_grouping_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_check_formulas_computation_group_idx" ON "p_rsf"."indicator_check_formulas" USING btree (
  "computation_group" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "unique_check_formula_title_udx" ON "p_rsf"."indicator_check_formulas" USING btree (
  normalizelabel(check_formula_title) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table indicator_check_formulas
-- ----------------------------
CREATE TRIGGER "trigger_remove_old_indicator_formula_label_id" AFTER DELETE ON "p_rsf"."indicator_check_formulas"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."remove_old_label_id"();
CREATE TRIGGER "trigger_set_indicator_check_formula_parameters" AFTER INSERT OR UPDATE OF "formula_indicator_ids", "formula", "formula_result_message", "check_formula_indicator_ids", "check_message_indicator_ids", "parent_grouping_pfcbl_category" ON "p_rsf"."indicator_check_formulas"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_indicator_check_formula_parameters"();
CREATE TRIGGER "trigger_set_indicator_check_ids" BEFORE INSERT OR UPDATE ON "p_rsf"."indicator_check_formulas"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_indicator_check_ids"();

-- ----------------------------
-- Uniques structure for table indicator_check_formulas
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_formulas" ADD CONSTRAINT "indicator_check_formulas_check_formula_id_indicator_check_i_key" UNIQUE ("check_formula_id", "indicator_check_id");
ALTER TABLE "p_rsf"."indicator_check_formulas" ADD CONSTRAINT "indicator_check_formulas_label_id_key" UNIQUE ("label_id");

-- ----------------------------
-- Checks structure for table indicator_check_formulas
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_formulas" ADD CONSTRAINT "check_formula_cannot_use_list_type_variables" CHECK ((formula ~ '\.all|\.intraperiod'::text) = false OR true);
ALTER TABLE "p_rsf"."indicator_check_formulas" ADD CONSTRAINT "system_use_delimiters_not_allowed_in_check_formula_titles" CHECK (NOT check_formula_title ~ '[#{}]'::text);
COMMENT ON CONSTRAINT "system_use_delimiters_not_allowed_in_check_formula_titles" ON "p_rsf"."indicator_check_formulas" IS 'template headers parse options will parse information inside curly brackets {} and within that, use # as a delimiter between data_value and data_unit, eg ''"IFC Maximum Risk Amount" US${facility_IFC_maximum_risk_amount#USD} as may be reduced from time to time in accordance with Section 2.05(b) (Costs);'' will parse the numeric value within the brackets and assign it (first) to the facility_IFC_maximum_risk_amount and after # assign constant USD to its data_value';

-- ----------------------------
-- Primary Key structure for table indicator_check_formulas
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_formulas" ADD CONSTRAINT "indicator_check_formulas_pkey" PRIMARY KEY ("check_formula_id");

-- ----------------------------
-- Indexes structure for table indicator_check_guidance
-- ----------------------------
CREATE INDEX "indicator_check_guidance_indicator_check_id_for_indicator_i_idx" ON "p_rsf"."indicator_check_guidance" USING btree (
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "for_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "indicator_check_guidance_unique_guidance_idx" ON "p_rsf"."indicator_check_guidance" USING btree (
  "for_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  normalizelabel(guidance) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table indicator_check_guidance
-- ----------------------------
CREATE TRIGGER "trigger_check_valid_guidance_pfcbl_category" BEFORE INSERT OR UPDATE ON "p_rsf"."indicator_check_guidance"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."check_valid_guidance_pfcbl_category"();
CREATE TRIGGER "trigger_guidance_check_counts_update" AFTER UPDATE ON "p_rsf"."indicator_check_guidance"
FOR EACH ROW
WHEN ((new.overwrite_check_class IS DISTINCT FROM old.overwrite_check_class))
EXECUTE PROCEDURE "p_rsf"."guidance_check_counts"();
CREATE TRIGGER "trigger_guidance_global_subscription" AFTER INSERT ON "p_rsf"."indicator_check_guidance"
FOR EACH ROW
WHEN ((new.for_pfcbl_category = 'global'::text))
EXECUTE PROCEDURE "p_rsf"."global_guidance_subscription"();

-- ----------------------------
-- Checks structure for table indicator_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_guidance" ADD CONSTRAINT "variance_is_zero_or_not_fraction_less_than_1" CHECK (variance_threshold = 0::numeric OR abs(variance_threshold) >= 1::numeric);
ALTER TABLE "p_rsf"."indicator_check_guidance" ADD CONSTRAINT "for_pfcbl_category_is_program_or_facility" CHECK (for_pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text]));
ALTER TABLE "p_rsf"."indicator_check_guidance" ADD CONSTRAINT "resolve_or_ignore_not_both" CHECK ((is_resolving_guidance AND is_ignoring_guidance) = false);
COMMENT ON CONSTRAINT "variance_is_zero_or_not_fraction_less_than_1" ON "p_rsf"."indicator_check_guidance" IS 'To ensure 15% is entered as 15 instead of 0.15';

-- ----------------------------
-- Primary Key structure for table indicator_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_guidance" ADD CONSTRAINT "indicator_check_guidance_pkey" PRIMARY KEY ("indicator_check_guidance_id");

-- ----------------------------
-- Checks structure for table indicator_check_types
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_types" ADD CONSTRAINT "valid_apply_on" CHECK (apply_on = ANY (ARRAY['data'::text, 'reporting'::text]));

-- ----------------------------
-- Primary Key structure for table indicator_check_types
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_types" ADD CONSTRAINT "indicator_check_types_pkey" PRIMARY KEY ("check_type");

-- ----------------------------
-- Indexes structure for table indicator_checks
-- ----------------------------
CREATE UNIQUE INDEX "indicator_checks_indicator_check_id_check_pfcbl_category_idx" ON "p_rsf"."indicator_checks" USING btree (
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
COMMENT ON INDEX "p_rsf"."indicator_checks_indicator_check_id_check_pfcbl_category_idx" IS 'for fk in indicator_check_formulas';
CREATE INDEX "indicator_checks_is_calculator_check_idx" ON "p_rsf"."indicator_checks" USING btree (
  "is_calculator_check" "pg_catalog"."bool_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table indicator_checks
-- ----------------------------
CREATE TRIGGER "trigger_cascade_check_subgrouping_changes_to_formulas" AFTER UPDATE ON "p_rsf"."indicator_checks"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."cascade_check_subgrouping_changes_to_formulas"();

-- ----------------------------
-- Uniques structure for table indicator_checks
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "indicator_checks_check_name_key" UNIQUE ("check_name");

-- ----------------------------
-- Checks structure for table indicator_checks
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "only_system_checks_have_null_pfcbl_category" CHECK (
CASE
    WHEN is_system THEN check_pfcbl_category IS NULL
    ELSE check_pfcbl_category IS NOT NULL
END);
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "indicator_checks_check" CHECK (
CASE
    WHEN auto_resolve_system_check IS NOT NULL THEN is_system = true
    ELSE true
END);
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "check_subgrouping_cannot_use_all_or_cumulative_variables" CHECK ((subgrouping::text ~ '\.all|\.cumulative'::text) = false);
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "check_valid_check_class" CHECK (check_class::text = ANY (ARRAY['critical'::character varying::text, 'error'::character varying::text, 'warning'::character varying::text, 'info'::character varying::text]));
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "check_valid_check_name" CHECK (check_name::text ~* '^[a-z0-9_]+$'::text);
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "only_system_checks_allow_variance" CHECK (
CASE
    WHEN is_system = false THEN variance_tolerance_allowed = false
    ELSE true
END);
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "calculator_checks_are_system_checks" CHECK (
CASE
    WHEN is_calculator_check = true THEN is_system = true
    ELSE true
END);
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "check_grouping_must_be_defined_if_subgrouping" CHECK (
CASE
    WHEN subgrouping IS NOT NULL THEN "grouping" IS NOT NULL
    ELSE true
END);

-- ----------------------------
-- Primary Key structure for table indicator_checks
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "indicator_checks_pkey" PRIMARY KEY ("indicator_check_id");

-- ----------------------------
-- Primary Key structure for table indicator_data_types
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_data_types" ADD CONSTRAINT "indicator_data_types_pkey" PRIMARY KEY ("data_type");

-- ----------------------------
-- Primary Key structure for table indicator_data_types_currency_units
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_data_types_currency_units" ADD CONSTRAINT "indicator_data_types_currency_units_pkey" PRIMARY KEY ("currency_code");

-- ----------------------------
-- Indexes structure for table indicator_formula_parameters
-- ----------------------------
CREATE INDEX "indicator_formula_parameters-indicator_id-idx" ON "p_rsf"."indicator_formula_parameters" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_formula_parameters-indicator_id_parameter_id-idx" ON "p_rsf"."indicator_formula_parameters" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parameter_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_formula_parameters-parameter_id-idx" ON "p_rsf"."indicator_formula_parameters" USING btree (
  "parameter_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_formula_parameters_indicator_id_parameter_pfcbl_h_idx" ON "p_rsf"."indicator_formula_parameters" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parameter_pfcbl_hierarchy" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_formula_parameters_parameter_indicator_id_paramet_idx" ON "p_rsf"."indicator_formula_parameters" USING btree (
  "parameter_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parameter_pfcbl_hierarchy" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Checks structure for table indicator_formula_parameters
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_formula_parameters" ADD CONSTRAINT "parameter_pfcbl_hierarchy_valid_values" CHECK (parameter_pfcbl_hierarchy = ANY (ARRAY['parent'::text, 'self'::text, 'child'::text]));

-- ----------------------------
-- Primary Key structure for table indicator_formula_parameters
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_formula_parameters" ADD CONSTRAINT "indicator_formula_parameters_pkey" PRIMARY KEY ("indicator_id", "parameter_indicator_id", "formula_id");

-- ----------------------------
-- Indexes structure for table indicator_formulas
-- ----------------------------
CREATE UNIQUE INDEX "each_indicator_formula_is_unique" ON "p_rsf"."indicator_formulas" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "formula" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "dep-formula_calculated_by_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "indicator_formulas-indicator_id-idx" ON "p_rsf"."indicator_formulas" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "only_one_global_default_allowed" ON "p_rsf"."indicator_formulas" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE is_primary_default = true;
CREATE UNIQUE INDEX "unique_calculation_formula_title_udx" ON "p_rsf"."indicator_formulas" USING btree (
  normalizelabel(formula_title) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table indicator_formulas
-- ----------------------------
CREATE TRIGGER "trigger_global_indicator_formulas_auto_subscribed" AFTER INSERT ON "p_rsf"."indicator_formulas"
FOR EACH ROW
WHEN ((new.formula_pfcbl_rank_range = ARRAY[(0)::smallint]))
EXECUTE PROCEDURE "p_rsf"."global_indicators_auto_subscribed"();
CREATE TRIGGER "trigger_reset_indicator_formula_ids" AFTER DELETE ON "p_rsf"."indicator_formulas"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."reset_indicator_formula_ids"();
CREATE TRIGGER "trigger_set_calculation_formula_parameters" AFTER INSERT OR UPDATE OF "formula", "formula_sort", "formula_indicator_ids" ON "p_rsf"."indicator_formulas"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_calculation_formula_parameters"();
CREATE TRIGGER "trigger_set_indicator_formula_id_ranks_inserted" AFTER INSERT ON "p_rsf"."indicator_formulas"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."set_indicator_formula_id_ranks"();
CREATE TRIGGER "trigger_set_indicator_formula_id_ranks_updated" AFTER UPDATE ON "p_rsf"."indicator_formulas"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."set_indicator_formula_id_ranks"();
CREATE TRIGGER "trigger_set_indicator_formula_ids" BEFORE INSERT OR UPDATE OF "formula_grouping_pfcbl_rank", "indicator_id", "modification_time", "formula", "formula_sort" ON "p_rsf"."indicator_formulas"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_indicator_formula_ids"();
CREATE TRIGGER "trigger_set_indicator_is_calculated" AFTER INSERT OR UPDATE OR DELETE ON "p_rsf"."indicator_formulas"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_indicator_is_calculated"();

-- ----------------------------
-- Uniques structure for table indicator_formulas
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "indicator_formulas_formula_id_indicator_id_key" UNIQUE ("formula_id", "indicator_id");

-- ----------------------------
-- Checks structure for table indicator_formulas
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "valid_formula_fx_date" CHECK (formula_fx_date = ANY (ARRAY['calculation'::text, 'parameter'::text, 'fx'::text, 'nofx'::text]));
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "dot_all_parameters_use_timeseries_values" CHECK (
CASE
    WHEN formula ~ '\.all'::text THEN formula ~ 'timeseries'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "disallowed_calculation_parameter_issuances" CHECK ((formula ~ '\.issuances'::text) = false);
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "valid_overwrite_type" CHECK (overwrite::text = ANY (ARRAY['allow'::character varying::text, 'deny'::character varying::text, 'missing'::character varying::text, 'unchanged'::character varying::text]));
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "system_use_delimiters_not_allowed_in_formula_titles" CHECK (NOT formula_title ~ '[#{}]'::text);
COMMENT ON CONSTRAINT "dot_all_parameters_use_timeseries_values" ON "p_rsf"."indicator_formulas" IS 'The .all parameter will return an embedded data.table object as a list within each row: the columns of the data.table are "timeseries", "timeseries.unit", "timeseries.reporteddate", "timeseries.changed", "timeseries.updated", "timeseries.reportnumber" and it is essential therefore that anyone using a .all parameter in a formula also uses a .timeseries value, since thats where the actual data is represented (and not using it means there is a formula error)';
COMMENT ON CONSTRAINT "disallowed_calculation_parameter_issuances" ON "p_rsf"."indicator_formulas" IS 'calculations can see all issuances and also the issuance group ID as needed for disaggregation, but are not expected to calculate values based on which issuances are within a series (or not); this is meaningful for checks';
COMMENT ON CONSTRAINT "system_use_delimiters_not_allowed_in_formula_titles" ON "p_rsf"."indicator_formulas" IS 'template headers parse options will parse information inside curly brackets {} and within that, use # as a delimiter between data_value and data_unit, eg ''"IFC Maximum Risk Amount" US${facility_IFC_maximum_risk_amount#USD} as may be reduced from time to time in accordance with Section 2.05(b) (Costs);'' will parse the numeric value within the brackets and assign it (first) to the facility_IFC_maximum_risk_amount and after # assign constant USD to its data_value';

-- ----------------------------
-- Primary Key structure for table indicator_formulas
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "indicator_formulas_pkey" PRIMARY KEY ("formula_id");

-- ----------------------------
-- Indexes structure for table indicator_object_logs
-- ----------------------------
CREATE UNIQUE INDEX "indicator_object_logs_table_name_table_id_log_date_idx" ON "p_rsf"."indicator_object_logs" USING btree (
  "table_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "table_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "log_date" "pg_catalog"."date_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table indicator_object_logs
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_object_logs" ADD CONSTRAINT "indicator_object_logs_pkey" PRIMARY KEY ("log_id");

-- ----------------------------
-- Indexes structure for table indicator_options_group_keys
-- ----------------------------
CREATE UNIQUE INDEX "indicator_options_group_keys_option_label_id_idx" ON "p_rsf"."indicator_options_group_keys" USING btree (
  "label_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "indicator_options_group_keys_options_group_id_option_key_idx" ON "p_rsf"."indicator_options_group_keys" USING btree (
  "options_group_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "options_group_key" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table indicator_options_group_keys
-- ----------------------------
CREATE TRIGGER "trigger_remove_old_options_group_keys_label_id" AFTER DELETE ON "p_rsf"."indicator_options_group_keys"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."remove_old_label_id"();
CREATE TRIGGER "trigger_set_options_group_keys_label_id" BEFORE INSERT ON "p_rsf"."indicator_options_group_keys"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_new_label_id"('indicator_options_group_keys');

-- ----------------------------
-- Primary Key structure for table indicator_options_group_keys
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_options_group_keys" ADD CONSTRAINT "indicator_options_group_keys_pkey" PRIMARY KEY ("options_group_id", "options_group_key", "label_id");

-- ----------------------------
-- Uniques structure for table indicator_options_groups
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_options_groups" ADD CONSTRAINT "indicator_options_groups_options_group_name_key" UNIQUE ("options_group_name");

-- ----------------------------
-- Checks structure for table indicator_options_groups
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_options_groups" ADD CONSTRAINT "check_valid_data_type" CHECK (options_group_data_type = ANY (ARRAY['text'::text, 'number'::text, 'date'::text, 'logical'::text]));
ALTER TABLE "p_rsf"."indicator_options_groups" ADD CONSTRAINT "check_valid_options_group_name" CHECK (options_group_name::text ~* '^[a-z0-9_]+'::text);

-- ----------------------------
-- Primary Key structure for table indicator_options_groups
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_options_groups" ADD CONSTRAINT "indicator_option_groups_pkey" PRIMARY KEY ("options_group_id");

-- ----------------------------
-- Primary Key structure for table indicator_sys_categories
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_sys_categories" ADD CONSTRAINT "indicator_id_categories_pkey" PRIMARY KEY ("indicator_sys_category");

-- ----------------------------
-- Indexes structure for table indicators
-- ----------------------------
CREATE INDEX "indicators_data_category_idx" ON "p_rsf"."indicators" USING btree (
  "data_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "indicators_indicator_id_indicator_sys_category_idx" ON "p_rsf"."indicators" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_sys_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "indicators_indicator_sys_category_idx" ON "p_rsf"."indicators" USING btree (
  "indicator_sys_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "indicators_label_id_idx" ON "p_rsf"."indicators" USING btree (
  "label_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "indicators_pfcbl_rank" ON "p_rsf"."indicators" USING btree (
  "pfcbl_rank" "pg_catalog"."int2_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "unique_fx_currency_pair_by_data_category" ON "p_rsf"."indicators" USING btree (
  "data_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  GREATEST("substring"(data_unit::text, '^[A-Z]{3}'::text), "substring"(data_unit::text, '[A-Z]{3}$'::text)) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  LEAST("substring"(data_unit::text, '^[A-Z]{3}'::text), "substring"(data_unit::text, '[A-Z]{3}$'::text)) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
) WHERE data_type::text = 'currency_ratio'::text;
CREATE UNIQUE INDEX "unique_indicator_sys_category_per_data_category" ON "p_rsf"."indicators" USING btree (
  "data_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "indicator_sys_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table indicators
-- ----------------------------
CREATE TRIGGER "trigger_cascade_indicator_name_changes_to_formulas" AFTER UPDATE ON "p_rsf"."indicators"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."cascade_indicator_name_changes_to_formulas"();
CREATE TRIGGER "trigger_global_indicators_auto_subscribed" AFTER INSERT ON "p_rsf"."indicators"
FOR EACH ROW
WHEN (((new.data_category)::text = 'global'::text))
EXECUTE PROCEDURE "p_rsf"."global_indicators_auto_subscribed"();
CREATE TRIGGER "trigger_indicator_currency_unit_valid" BEFORE INSERT OR UPDATE ON "p_rsf"."indicators"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."indicator_currency_unit_valid"();
CREATE TRIGGER "trigger_remove_old_indicator_label_id" AFTER DELETE ON "p_rsf"."indicators"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."remove_old_label_id"();
CREATE TRIGGER "trigger_set_indicator_label_id" BEFORE INSERT ON "p_rsf"."indicators"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_new_label_id"('indicators');

-- ----------------------------
-- Uniques structure for table indicators
-- ----------------------------
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_indicator_name_key" UNIQUE ("indicator_name");

-- ----------------------------
-- Checks structure for table indicators
-- ----------------------------
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "is_setup_requires_default_subscription" CHECK (
CASE
    WHEN is_setup IS NOT NULL THEN default_subscription IS TRUE
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "is_data_unit_has_valid_sys_category" CHECK (
CASE
    WHEN is_data_unit = true THEN indicator_sys_category::text ~ 'currency_unit$'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "check_valid_indicator_name" CHECK (indicator_name::text ~* '^[a-zA-Z0-9_]+$'::text);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "check_indicator_name_includes_data_category" CHECK (indicator_name::text ~* (('^(sys_)?'::text || data_category::text) || '_.*$'::text) OR indicator_name::text ~* '^rsf_new_indicator.*$'::text);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "check_is_system_calculated_uses_system_and_calculated" CHECK (
CASE
    WHEN is_system_calculated = true THEN is_system = true AND is_calculated = true
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "check_options_group_allows_multiples_can_only_be_text" CHECK (COALESCE(indicator_options_group_allows_multiples, false) = true AND data_type::text = 'text'::text OR COALESCE(indicator_options_group_allows_multiples, false) = false);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currenct_ratio_LCU_disallowed_for_global_indicators" CHECK (
CASE
    WHEN data_category::text = 'global'::text AND data_type::text = 'currency_ratio'::text THEN (data_unit::text ~* 'LCU'::text) = false
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_data_type_units_format" CHECK (
CASE
    WHEN data_type::text = 'currency'::text THEN data_unit IS NOT NULL AND data_unit::text ~ '^[A-Z]{3}$'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_has_data_unit" CHECK (data_category::text = 'currency'::text AND data_unit IS NOT NULL OR NOT data_category::text = 'currency'::text);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_data_type_LCU_must_be_denominator" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text AND data_unit::text ~* 'LCU'::text THEN data_unit::text ~* '^[A-Z]{3}/LCU$'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_data_type_is_calculated" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text THEN is_calculated
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_data_type_ratio_alphabetic_order_if_not_LCU" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text THEN data_unit::text ~* 'LCU'::text OR p_rsf.fx_currency_ratio_has_alphabetic_order(data_unit::text)
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_data_type_units_format" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text THEN data_unit IS NOT NULL AND data_unit::text ~ '^[A-Z]{3}/[A-Z]{3}$'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "data_unit_is_null_or_has_length" CHECK (data_unit IS NULL OR char_length(btrim(data_unit::text)) > 0);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "data_unit_is_upper_case" CHECK (data_unit IS NULL OR data_unit::text ~ '^[A-Z0-9/]+$'::text);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "entity_currency_unit_data_type_is_lcu" CHECK (
CASE
    WHEN indicator_sys_category::text = 'entity_currency_unit'::text THEN data_unit::text = 'LCU'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "is_data_unit_has_default_value" CHECK (
CASE
    WHEN is_data_unit = true THEN data_unit IS NULL AND default_value IS NOT NULL
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "is_data_unit_is_text" CHECK (
CASE
    WHEN is_data_unit = true THEN data_type::text = 'text'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "is_setup_valid_values" CHECK (is_setup IS NULL OR (is_setup = ANY (ARRAY['required'::text, 'optional'::text])));
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "lcu_is_defined_by_facility_only" CHECK (
CASE
    WHEN indicator_sys_category::text = 'entity_local_currency_unit'::text THEN data_category::text = ANY (ARRAY['program'::character varying::text, 'facility'::character varying::text])
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "only_numeric_data_types_have_units" CHECK (
CASE
    WHEN data_type::text = ANY (ARRAY['text'::character varying::text, 'logical'::character varying::text, 'date'::character varying::text]) THEN data_unit IS NULL
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicator_names_start_with_lower_case_letters" CHECK ((indicator_name::text ~ '^[A-Z]'::text) = false);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_allowed_data_categories" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text THEN data_category::text = ANY (ARRAY['global'::character varying::text, 'facility'::character varying::text])
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "system_indicators_are_default_subscriable" CHECK (
CASE
    WHEN is_system = true THEN default_subscription IS TRUE
    ELSE true
END);
COMMENT ON CONSTRAINT "is_data_unit_has_valid_sys_category" ON "p_rsf"."indicators" IS 'Presently, only currencies can be defined by is_data_unit (perhaps this will change in the future, and would require updates in template_parse_formats)';
COMMENT ON CONSTRAINT "check_valid_indicator_name" ON "p_rsf"."indicators" IS 'indicator_name meets syntax';
COMMENT ON CONSTRAINT "check_indicator_name_includes_data_category" ON "p_rsf"."indicators" IS 'Useful for human interpretation of the indicator; but also necessary for system to resolve the indicator''s data category from the name itself.';
COMMENT ON CONSTRAINT "check_options_group_allows_multiples_can_only_be_text" ON "p_rsf"."indicators" IS 'Multiple selections stored as &-delimited text, so options group that are numerics, etc will throw errors within the application when checking/enforcing that data is appropriately typed.  Meanwhile, fully checking all potential data being a multi-select and type introduces extreme complexity.  Text-only multiples is a fair compromise and only indicator level calculations or checks need to worry about parsing concatenated values';
COMMENT ON CONSTRAINT "currency_has_data_unit" ON "p_rsf"."indicators" IS 'Currency must specify a currency unit (LCU if not currency specific)';
COMMENT ON CONSTRAINT "currency_ratio_data_type_LCU_must_be_denominator" ON "p_rsf"."indicators" IS 'Global program cannot define an LCU value conceptually; and technically, sys_global_fx* indicators will always lookup and define actual currencies, so disallowing LCU ensures consistency';
COMMENT ON CONSTRAINT "currency_ratio_data_type_is_calculated" ON "p_rsf"."indicators" IS 'Currecy ratio data types must be calcualted to ensure that the currency ratio is (re)validated each reporting period.  Pegged currencies will have a fixed fx rate to the currency they are pegged to and will not change over time; whereas floating currencies will almost certainly change from quarter to quarter.  So enforcing a calculation will ensure this is checked and also that a flag will be raised if a user forgets to update an fx rate that should be updated.';
COMMENT ON CONSTRAINT "currency_ratio_data_type_ratio_alphabetic_order_if_not_LCU" ON "p_rsf"."indicators" IS 'Currency ratios are entered in alphabetic order, unless LCU is used in the denominator';
COMMENT ON CONSTRAINT "currency_ratio_data_type_units_format" ON "p_rsf"."indicators" IS 'Entity currency unit must define a currency unit value';
COMMENT ON CONSTRAINT "is_data_unit_is_text" ON "p_rsf"."indicators" IS 'Data units must be text data types.';
COMMENT ON CONSTRAINT "indicator_names_start_with_lower_case_letters" ON "p_rsf"."indicators" IS 'System dashboard uses upper case letters to identify system columns';
COMMENT ON CONSTRAINT "currency_ratio_allowed_data_categories" ON "p_rsf"."indicators" IS 'Currency ratios (ie, fx rates) can only be defined at global (which is IFC official system rates); or at facility levels -- rate sources defined by and provided by the respective facilities per contractual agreement';

-- ----------------------------
-- Primary Key structure for table indicators
-- ----------------------------
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_pkey" PRIMARY KEY ("indicator_id");

-- ----------------------------
-- Primary Key structure for table label_ids
-- ----------------------------
ALTER TABLE "p_rsf"."label_ids" ADD CONSTRAINT "label_ids_pkey" PRIMARY KEY ("label_id");

-- ----------------------------
-- Uniques structure for table label_keys
-- ----------------------------
ALTER TABLE "p_rsf"."label_keys" ADD CONSTRAINT "label_keys_label_key_name_key" UNIQUE ("label_key_name");

-- ----------------------------
-- Checks structure for table label_keys
-- ----------------------------
ALTER TABLE "p_rsf"."label_keys" ADD CONSTRAINT "key_type_is_valid" CHECK (key_type::text = ANY (ARRAY['language'::character varying::text, 'client'::character varying::text, 'template'::character varying::text, 'ifc'::text]));
ALTER TABLE "p_rsf"."label_keys" ADD CONSTRAINT "type_template_has_id" CHECK (key_type::text = 'template'::text AND key_type_template_id IS NOT NULL OR key_type::text <> 'template'::text);

-- ----------------------------
-- Primary Key structure for table label_keys
-- ----------------------------
ALTER TABLE "p_rsf"."label_keys" ADD CONSTRAINT "label_keys_pkey" PRIMARY KEY ("label_key");

-- ----------------------------
-- Indexes structure for table labels
-- ----------------------------
CREATE UNIQUE INDEX "labels_label_key_primary_label_label_id_group_idx" ON "p_rsf"."labels" USING btree (
  "label_key" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  btrim(regexp_replace(f_unaccent(lower(primary_label)), '[[:space:]]+'::text, ''::text, 'g'::text)) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "label_id_group" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "labels_unique_indicators_primary_label" ON "p_rsf"."labels" USING btree (
  "label_key" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  btrim(regexp_replace(f_unaccent(lower(primary_label)), '[[:space:]]+'::text, ''::text, 'g'::text)) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "label_id_group" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table labels
-- ----------------------------
CREATE TRIGGER "trigger_cascade_label_id_group" BEFORE INSERT ON "p_rsf"."labels"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."cascade_label_id_group"();
CREATE TRIGGER "trigger_normalize_labels" BEFORE INSERT OR UPDATE ON "p_rsf"."labels"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."normalize_labels"();

-- ----------------------------
-- Checks structure for table labels
-- ----------------------------
ALTER TABLE "p_rsf"."labels" ADD CONSTRAINT "check_commas_or_ampersand_not_allowed_in_options_labels" CHECK (true);
COMMENT ON CONSTRAINT "check_commas_or_ampersand_not_allowed_in_options_labels" ON "p_rsf"."labels" IS '*DISABLED: (label_id_group::text ~* ''^options-.*$''::text) = true AND (array_to_string(ARRAY[primary_label] || secondary_labels, '' ''::text) ~* ''[,&]''::text) = false OR (label_id_group::text ~* ''^options-.*$''::text) = false';

-- ----------------------------
-- Primary Key structure for table labels
-- ----------------------------
ALTER TABLE "p_rsf"."labels" ADD CONSTRAINT "labels_pkey" PRIMARY KEY ("label_id", "label_key");

-- ----------------------------
-- Primary Key structure for table program_settings
-- ----------------------------
ALTER TABLE "p_rsf"."program_settings" ADD CONSTRAINT "program_settings_pkey" PRIMARY KEY ("setting_name");

-- ----------------------------
-- Primary Key structure for table reporting_cohort_info
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_cohort_info" ADD CONSTRAINT "reporting_cohort_uploads_pkey" PRIMARY KEY ("reporting_cohort_id");

-- ----------------------------
-- Indexes structure for table reporting_cohorts
-- ----------------------------
CREATE INDEX "reporting_cohorts-rsf_program_id-where-reported_idx" ON "p_rsf"."reporting_cohorts" USING btree (
  "!dep-rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE is_reported_cohort = true;
CREATE INDEX "reporting_cohorts_from_reporting_template_id_idx" ON "p_rsf"."reporting_cohorts" USING btree (
  "!dep-from_reporting_template_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "reporting_cohorts_parent_reporting_cohort_id_idx" ON "p_rsf"."reporting_cohorts" USING btree (
  "!dep-parent_reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "reporting_cohorts_reporting_cohort_id_reporting_asof_date_idx" ON "p_rsf"."reporting_cohorts" USING btree (
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "reporting_cohorts_reporting_cohort_id_reporting_asof_date_r_idx" ON "p_rsf"."reporting_cohorts" USING btree (
  "reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "reporting_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "reporting_cohorts_rsf_facility_id_idx" ON "p_rsf"."reporting_cohorts" USING btree (
  "!dep-rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "!dep-rsf_facility_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "reporting_cohorts_rsf_program_id_reporting_asof_date_idx" ON "p_rsf"."reporting_cohorts" USING btree (
  "!dep-rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table reporting_cohorts
-- ----------------------------
CREATE TRIGGER "trigger_global_reporting" AFTER INSERT ON "p_rsf"."reporting_cohorts"
FOR EACH ROW
WHEN (((new.is_reported_cohort = true) AND (new.reporting_rsf_pfcbl_id <> 0)))
EXECUTE PROCEDURE "p_rsf"."global_reporting"();
CREATE TRIGGER "trigger_reporting_cohort_group_deleted" AFTER DELETE ON "p_rsf"."reporting_cohorts"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."reporting_cohort_group_deleted"();
CREATE TRIGGER "trigger_reporting_cohorts_validate_permissions" BEFORE INSERT ON "p_rsf"."reporting_cohorts"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."reporting_cohorts_validate_permissions"();

-- ----------------------------
-- Uniques structure for table reporting_cohorts
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "reporting_cohorts_reporting_cohort_id_from_reporting_templa_key" UNIQUE ("reporting_cohort_id", "!dep-from_reporting_template_id");
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "reporting_cohorts_reporting_cohort_id_is_reported_cohort_key" UNIQUE ("reporting_cohort_id", "is_reported_cohort");
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "unique_reporting_time" UNIQUE ("reporting_time");
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "linked_reporting_cohort_unique_has_one_link_in_time" UNIQUE ("reporting_asof_date", "!dep-linked_reporting_cohort_id", "!dep-rsf_program_id", "!dep-rsf_facility_id");
COMMENT ON CONSTRAINT "linked_reporting_cohort_unique_has_one_link_in_time" ON "p_rsf"."reporting_cohorts" IS 'Linked cohorts can only have one foreign (future or past) reporting_asof_date linked to it';

-- ----------------------------
-- Checks structure for table reporting_cohorts
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "data_asof_date_same_quarter_as_reporting_asof_date" CHECK (data_asof_date >= date_trunc('quarter'::text, reporting_asof_date::timestamp with time zone)::date AND data_asof_date <= reporting_asof_date);
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "linked_cohorts_do_not_have_parents" CHECK (
CASE
    WHEN "!dep-linked_reporting_cohort_id" IS NOT NULL THEN "!dep-parent_reporting_cohort_id" IS NULL
    ELSE true
END);
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "cannot_be_calculated_and_intraperiod" CHECK (
CASE
    WHEN "!dep-is_redundancy_cohort" = true THEN is_calculated_cohort = false
    ELSE true
END);
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "cannot_be_calculated_and_reported" CHECK (
CASE
    WHEN is_reported_cohort = true THEN is_calculated_cohort = false
    ELSE true
END);
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "cannot_be_own_parent" CHECK ("!dep-parent_reporting_cohort_id" <> reporting_cohort_id);

-- ----------------------------
-- Primary Key structure for table reporting_cohorts
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "rsf_reporting_cohorts_pkey" PRIMARY KEY ("reporting_cohort_id");

-- ----------------------------
-- Primary Key structure for table reporting_import_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_import_template_headers" ADD CONSTRAINT "reporting_import_template_headers_pkey" PRIMARY KEY ("import_id", "rsf_pfcbl_id", "indicator_id", "template_header", "template_header_position");

-- ----------------------------
-- Indexes structure for table reporting_imports
-- ----------------------------
CREATE INDEX "reporting_imports-import_rsf_pfcbl_id" ON "p_rsf"."reporting_imports" USING btree (
  "import_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "reporting_imports-import_rsf_pfcbl_id-reporting_asof_date" ON "p_rsf"."reporting_imports" USING btree (
  "import_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table reporting_imports
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_imports" ADD CONSTRAINT "import_templates_pkey" PRIMARY KEY ("import_id");

-- ----------------------------
-- Triggers structure for table reporting_imports_deleted_archive
-- ----------------------------
CREATE TRIGGER "trigger_deleted_reporting_imports_action" AFTER INSERT ON "p_rsf"."reporting_imports_deleted_archive"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."deleted_reporting_imports_action"();
CREATE TRIGGER "trigger_deleted_reporting_imports_logging" BEFORE INSERT ON "p_rsf"."reporting_imports_deleted_archive"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."deleted_reporting_imports_logging"();

-- ----------------------------
-- Indexes structure for table reporting_templates
-- ----------------------------
CREATE UNIQUE INDEX "reporting_templates_template_key_idx" ON "p_rsf"."reporting_templates" USING btree (
  "template_key" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "reporting_templates_template_name_idx" ON "p_rsf"."reporting_templates" USING btree (
  "template_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table reporting_templates
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_templates" ADD CONSTRAINT "reporting_templates_pkey" PRIMARY KEY ("template_id");

-- ----------------------------
-- Primary Key structure for table reports
-- ----------------------------
ALTER TABLE "p_rsf"."reports" ADD CONSTRAINT "reports_pkey" PRIMARY KEY ("report_id");

-- ----------------------------
-- Indexes structure for table rsf_clients
-- ----------------------------
CREATE INDEX "rsf_clients-rsf_facility_id_idx" ON "p_rsf"."rsf_clients" USING btree (
  "rsf_facility_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."rsf_clients" CLUSTER ON "rsf_clients-rsf_facility_id_idx";

-- ----------------------------
-- Primary Key structure for table rsf_clients
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_clients" ADD CONSTRAINT "rsf_facility_clients_pkey" PRIMARY KEY ("rsf_client_id");

-- ----------------------------
-- Cluster option for table rsf_clients
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_clients" CLUSTER ON "rsf_clients-rsf_facility_id_idx";

-- ----------------------------
-- Indexes structure for table rsf_data
-- ----------------------------
CREATE INDEX "rsf_data-indicator_id_idx" ON "p_rsf"."rsf_data" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data-reporting_cohort_id_idx" ON "p_rsf"."rsf_data" USING btree (
  "reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data-rsf_pfcbl_id&indicator_id_idx" ON "p_rsf"."rsf_data" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data-rsf_pfcbl_id_idx" ON "p_rsf"."rsf_data" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."rsf_data" CLUSTER ON "rsf_data-rsf_pfcbl_id_idx";

-- ----------------------------
-- Triggers structure for table rsf_data
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_0_inserted_data_unit_lcu" BEFORE INSERT ON "p_rsf"."rsf_data"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_inserted_data_unit_lcu"();
CREATE TRIGGER "trigger_rsf_data_1_inserted_data_integrity" AFTER INSERT ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_inserted_data_integrity"();
CREATE TRIGGER "trigger_rsf_data_2_deleted_pfcbl_reporting" AFTER DELETE ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_deleted_pfcbl_reporting"();
CREATE TRIGGER "trigger_rsf_data_2_inserted_pfcbl_reporting" AFTER INSERT ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_inserted_pfcbl_reporting"();
CREATE TRIGGER "trigger_rsf_data_3_deleted_data_current" AFTER DELETE ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_deleted_data_current"();
CREATE TRIGGER "trigger_rsf_data_3_inserted_data_current" AFTER INSERT ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_inserted_data_current"();
CREATE TRIGGER "trigger_rsf_data_3_modified_sys_flags" AFTER UPDATE OF "data_sys_flags" ON "p_rsf"."rsf_data"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_sys_flags"();
CREATE TRIGGER "trigger_rsf_data_4_deleted_calculations" AFTER DELETE ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_calculations"();
CREATE TRIGGER "trigger_rsf_data_4_inserted_calculations" AFTER INSERT ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_calculations"();
CREATE TRIGGER "trigger_rsf_data_4_updated_calculations" AFTER UPDATE ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_calculations"();
CREATE TRIGGER "trigger_rsf_data_5_deleted_checks" AFTER DELETE ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_checks"();
CREATE TRIGGER "trigger_rsf_data_5_inserted_checks" AFTER INSERT ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_checks"();
CREATE TRIGGER "trigger_rsf_data_5_updated_checks" AFTER UPDATE ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_checks"();

-- ----------------------------
-- Uniques structure for table rsf_data
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data" ADD CONSTRAINT "rsf_data_rsf_reporting_id_indicator_id_update_time_key" UNIQUE ("rsf_pfcbl_id", "indicator_id", "reporting_cohort_id");

-- ----------------------------
-- Primary Key structure for table rsf_data
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data" ADD CONSTRAINT "rsf_data_pkey" PRIMARY KEY ("data_id");

-- ----------------------------
-- Cluster option for table rsf_data
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data" CLUSTER ON "rsf_data-rsf_pfcbl_id_idx";

-- ----------------------------
-- Indexes structure for table rsf_data_calculation_evaluations
-- ----------------------------
CREATE INDEX "calculation_evaluations-rsf_pfcbl_id&indicator_id" ON "p_rsf"."rsf_data_calculation_evaluations" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "calculation_evaluations-rsf_pfcbl_id_idx" ON "p_rsf"."rsf_data_calculation_evaluations" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."rsf_data_calculation_evaluations" CLUSTER ON "calculation_evaluations-rsf_pfcbl_id_idx";

-- ----------------------------
-- Triggers structure for table rsf_data_calculation_evaluations
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_calculation_evaluation_allow" BEFORE INSERT ON "p_rsf"."rsf_data_calculation_evaluations"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_calculation_evaluation_allowed"();
CREATE TRIGGER "trigger_rsf_data_calculation_evaluation_validation" BEFORE DELETE ON "p_rsf"."rsf_data_calculation_evaluations"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_calculation_evaluation_validation"();

-- ----------------------------
-- Primary Key structure for table rsf_data_calculation_evaluations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_calculation_evaluations" ADD CONSTRAINT "rsf_data_calculation_evaluations_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_id", "calculation_asof_date");

-- ----------------------------
-- Cluster option for table rsf_data_calculation_evaluations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_calculation_evaluations" CLUSTER ON "calculation_evaluations-rsf_pfcbl_id_idx";

-- ----------------------------
-- Primary Key structure for table rsf_data_calculation_validations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_calculation_validations" ADD CONSTRAINT "rsf_data_calculation_validations_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_id", "calculation_asof_date");

-- ----------------------------
-- Indexes structure for table rsf_data_check_evaluations
-- ----------------------------
CREATE INDEX "check_evaluations-rsf_pfcbl_id&indicator_id&check_id" ON "p_rsf"."rsf_data_check_evaluations" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "check_evaluations-rsf_pfcbl_id_idx" ON "p_rsf"."rsf_data_check_evaluations" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."rsf_data_check_evaluations" CLUSTER ON "check_evaluations-rsf_pfcbl_id_idx";

-- ----------------------------
-- Primary Key structure for table rsf_data_check_evaluations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_check_evaluations" ADD CONSTRAINT "rsf_data_check_evaluations_pkey" PRIMARY KEY ("rsf_pfcbl_id", "check_asof_date", "check_formula_id");

-- ----------------------------
-- Cluster option for table rsf_data_check_evaluations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_check_evaluations" CLUSTER ON "check_evaluations-rsf_pfcbl_id_idx";

-- ----------------------------
-- Indexes structure for table rsf_data_checks
-- ----------------------------
CREATE INDEX "archive_sys_name_collate_c_and_date_and_check" ON "p_rsf"."rsf_data_checks" USING btree (
  "archive_sys_name" COLLATE "pg_catalog"."C" "pg_catalog"."text_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "data_value_unit" COLLATE "pg_catalog"."C" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-current-data_id" ON "p_rsf"."rsf_data_checks" USING btree (
  "data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE check_data_id_is_current = true;
CREATE INDEX "rsf_data_checks-data_id&check_asof_date_ifx" ON "p_rsf"."rsf_data_checks" USING btree (
  "data_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-data_id_idx" ON "p_rsf"."rsf_data_checks" USING btree (
  "data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "rsf_data_checks-formula_application_unique_per_entity_date-udx" ON "p_rsf"."rsf_data_checks" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE check_data_id_is_current = true AND check_formula_id IS NOT NULL;
COMMENT ON INDEX "p_rsf"."rsf_data_checks-formula_application_unique_per_entity_date-udx" IS 'For non-null formula_ids (ie, user checks, not system checks where formula_id will be NULL), then ensure each rsf_pfcbl_id+check_asof_date+check_formula_id is uniquely applied to ensure that data updates do not cause the same check to be applied on multiple different indicators ';
CREATE INDEX "rsf_data_checks-pfcbl&indicator&check_asof_date_idx" ON "p_rsf"."rsf_data_checks" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-pfcbl&indicator_idx" ON "p_rsf"."rsf_data_checks" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_data_checks
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_checks_0_deleted" AFTER DELETE ON "p_rsf"."rsf_data_checks"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_deleted_archive"();
CREATE TRIGGER "trigger_rsf_data_checks_1_modified_flagged" BEFORE INSERT OR UPDATE OF "data_sys_flags" ON "p_rsf"."rsf_data_checks"
FOR EACH ROW
WHEN ((new.data_sys_flags IS NOT NULL))
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_set_archivable"();
CREATE TRIGGER "trigger_rsf_data_checks_2_flagged" AFTER INSERT OR UPDATE OF "data_sys_flags" ON "p_rsf"."rsf_data_checks"
FOR EACH ROW
WHEN ((new.data_sys_flags IS NOT NULL))
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_flagged_data_cascade"();
CREATE TRIGGER "trigger_rsf_data_checks_2_updated_comment" BEFORE UPDATE OF "check_status_comment" ON "p_rsf"."rsf_data_checks"
FOR EACH ROW
WHEN ((new.check_status_comment IS NOT NULL))
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_set_archivable"();
CREATE TRIGGER "trigger_rsf_data_checks_validate_permissions" AFTER UPDATE OF "check_status" ON "p_rsf"."rsf_data_checks"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_validate_permissions"();

-- ----------------------------
-- Checks structure for table rsf_data_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "check_ignore_requires_comment" CHECK (
CASE
    WHEN check_ignore = true THEN check_status_comment IS NOT NULL AND check_status_user_id IS NOT NULL
    ELSE check_ignore = false
END);
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "check_resolved_requires_comment" CHECK (
CASE
    WHEN check_status = 'resolved'::text THEN check_status_comment IS NOT NULL AND check_status_user_id IS NOT NULL
    ELSE check_status = 'active'::text
END);
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "check_status_active_or_resolved" CHECK (check_status = ANY (ARRAY['active'::text, 'resolved'::text]));
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "ignored_checks_are_resolved" CHECK (
CASE
    WHEN check_ignore = true THEN check_status = 'resolved'::text
    ELSE check_ignore = false
END);

-- ----------------------------
-- Primary Key structure for table rsf_data_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_evaluation_id_pkey" PRIMARY KEY ("evaluation_id");

-- ----------------------------
-- Indexes structure for table rsf_data_checks_archive
-- ----------------------------
CREATE INDEX "sys_name_date_check_id_idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "sys_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "sys_name_idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "sys_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Uniques structure for table rsf_data_checks_archive
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "no_duplicate_archive_flags" UNIQUE ("sys_name", "indicator_id", "indicator_check_id", "check_asof_date", "check_status", "check_status_user_id", "check_status_comment", "check_message", "consolidated_from_indicator_id", "consolidated_from_indicator_check_id", "data_sys_flags", "data_value_unit");

-- ----------------------------
-- Primary Key structure for table rsf_data_checks_archive
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_pkey" PRIMARY KEY ("archive_id");

-- ----------------------------
-- Indexes structure for table rsf_data_current
-- ----------------------------
CREATE UNIQUE INDEX "rsf_data_current-data_id_udx" ON "p_rsf"."rsf_data_current" USING btree (
  "data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current-data_unit_data_id_idx" ON "p_rsf"."rsf_data_current" USING btree (
  "data_unit_data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current-rsf_pfcbl_id_indicator_id_idx" ON "p_rsf"."rsf_data_current" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."rsf_data_current" CLUSTER ON "rsf_data_current-rsf_pfcbl_id_indicator_id_idx";

-- ----------------------------
-- Triggers structure for table rsf_data_current
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_current_0_modified_unchanged" BEFORE INSERT OR UPDATE ON "p_rsf"."rsf_data_current"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_modified_unchanged"();
CREATE TRIGGER "trigger_rsf_data_current_1_modified_names_and_ids_deleted" AFTER DELETE ON "p_rsf"."rsf_data_current"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_names_and_ids_modified"();
CREATE TRIGGER "trigger_rsf_data_current_1_modified_names_and_ids_inserted" AFTER INSERT ON "p_rsf"."rsf_data_current"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_names_and_ids_modified"();
CREATE TRIGGER "trigger_rsf_data_current_1_modified_names_and_ids_updated" AFTER UPDATE ON "p_rsf"."rsf_data_current"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_names_and_ids_modified"();
CREATE TRIGGER "trigger_rsf_data_current_2_modified_fx_deleted" AFTER DELETE ON "p_rsf"."rsf_data_current"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_fx_modified"();
CREATE TRIGGER "trigger_rsf_data_current_2_modified_fx_updated" AFTER UPDATE ON "p_rsf"."rsf_data_current"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_fx_modified"();

-- ----------------------------
-- Primary Key structure for table rsf_data_current
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current" ADD CONSTRAINT "rsf_data_current_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_id", "reporting_asof_date");

-- ----------------------------
-- Cluster option for table rsf_data_current
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current" CLUSTER ON "rsf_data_current-rsf_pfcbl_id_indicator_id_idx";

-- ----------------------------
-- Indexes structure for table rsf_data_current_fx
-- ----------------------------
CREATE INDEX "rsf_data_current_fx-entity_indicator_date-idx" ON "p_rsf"."rsf_data_current_fx" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
COMMENT ON INDEX "p_rsf"."rsf_data_current_fx-entity_indicator_date-idx" IS 'An individual entity-indicator-date might be the calculated aggregate of multiple different currencies and therefore could have multiple FX IDs per index entry';
CREATE INDEX "rsf_data_current_fx-fx_data_id-idx" ON "p_rsf"."rsf_data_current_fx" USING btree (
  "fx_data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current_fx-rsf_pfcbl_id-idx" ON "p_rsf"."rsf_data_current_fx" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table rsf_data_current_fx
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_fx" ADD CONSTRAINT "rsf_data_current_fx_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_id", "reporting_asof_date", "fx_data_id");

-- ----------------------------
-- Indexes structure for table rsf_data_current_lcu
-- ----------------------------
CREATE INDEX "rsf_data_current_lcu-for_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_data_current_lcu" USING btree (
  "for_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current_lcu-lcu_unit_data_id_idx" ON "p_rsf"."rsf_data_current_lcu" USING btree (
  "lcu_unit_data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_data_current_lcu
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_current_lcu_1_deleted" AFTER DELETE ON "p_rsf"."rsf_data_current_lcu"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_lcu_modified"();
CREATE TRIGGER "trigger_rsf_data_current_lcu_1_inserted" AFTER INSERT ON "p_rsf"."rsf_data_current_lcu"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_lcu_modified"();
CREATE TRIGGER "trigger_rsf_data_current_lcu_1_updated" AFTER UPDATE ON "p_rsf"."rsf_data_current_lcu"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_lcu_modified"();

-- ----------------------------
-- Checks structure for table rsf_data_current_lcu
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_lcu" ADD CONSTRAINT "valid_currency_format" CHECK (data_unit_value ~ '^[A-Z]{3}$'::text);

-- ----------------------------
-- Primary Key structure for table rsf_data_current_lcu
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_lcu" ADD CONSTRAINT "rsf_data_current_lcu_pkey" PRIMARY KEY ("for_rsf_pfcbl_id", "reporting_asof_date");

-- ----------------------------
-- Indexes structure for table rsf_data_current_names_and_ids
-- ----------------------------
CREATE INDEX "rsf_data_current_names_and_ids_id_pfcbl_category_idx" ON "p_rsf"."rsf_data_current_names_and_ids" USING btree (
  "id" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current_names_and_ids_pfcbl_name_idx" ON "p_rsf"."rsf_data_current_names_and_ids" USING btree (
  "pfcbl_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current_names_and_ids_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_data_current_names_and_ids" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "rsf_data_current_names_and_ids_sys_name_udx" ON "p_rsf"."rsf_data_current_names_and_ids" USING btree (
  "sys_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
COMMENT ON INDEX "p_rsf"."rsf_data_current_names_and_ids_sys_name_udx" IS 'Clients will sometimes make typos in client names, then correct them later, creating timeseries repetitions in names';
CREATE INDEX "rsf_data_current_names_nad_ids_sys_name_gin" ON "p_rsf"."rsf_data_current_names_and_ids" USING gin (
  "sys_name" COLLATE "pg_catalog"."default" "p_rsf"."gin_trgm_ops"
);

-- ----------------------------
-- Checks structure for table rsf_data_current_names_and_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_names_and_ids" ADD CONSTRAINT "greater_than_not_allowed_in_names" CHECK ((pfcbl_name ~ '>'::text) = false);
ALTER TABLE "p_rsf"."rsf_data_current_names_and_ids" ADD CONSTRAINT "comma_not_allowed_in_names" CHECK ((pfcbl_name ~ ','::text) = false);

-- ----------------------------
-- Primary Key structure for table rsf_data_current_names_and_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_names_and_ids" ADD CONSTRAINT "rsf_data_current_names_and_ids_pkey" PRIMARY KEY ("rsf_pfcbl_id", "reporting_asof_date");

-- ----------------------------
-- Indexes structure for table rsf_facilities
-- ----------------------------
CREATE INDEX "rsf_facilities-rsf_program_id_idx" ON "p_rsf"."rsf_facilities" USING btree (
  "rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."rsf_facilities" CLUSTER ON "rsf_facilities-rsf_program_id_idx";

-- ----------------------------
-- Primary Key structure for table rsf_facilities
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_facilities" ADD CONSTRAINT "rsf_program_facilities_pkey" PRIMARY KEY ("rsf_facility_id");

-- ----------------------------
-- Cluster option for table rsf_facilities
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_facilities" CLUSTER ON "rsf_facilities-rsf_program_id_idx";

-- ----------------------------
-- Indexes structure for table rsf_loan_issuance_series
-- ----------------------------
CREATE INDEX "rsf_loan_issuance_series_id_value_data_id_idx" ON "p_rsf"."rsf_loan_issuance_series" USING btree (
  "id_value_data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_loan_issuance_series_loan_issuance_series_id_idx" ON "p_rsf"."rsf_loan_issuance_series" USING btree (
  "loan_issuance_series_id" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_loan_issuance_series_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_loan_issuance_series" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_loan_issuance_series
-- ----------------------------
CREATE TRIGGER "trigger_rsf_loan_issuance_series_1_deleted_obsolete" AFTER DELETE ON "p_rsf"."rsf_loan_issuance_series"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_loan_issuance_series_obsolete"();

-- ----------------------------
-- Primary Key structure for table rsf_loan_issuance_series
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_loan_issuance_series" ADD CONSTRAINT "rsf_loan_issuance_series_pkey" PRIMARY KEY ("rsf_pfcbl_id");

-- ----------------------------
-- Uniques structure for table rsf_pfcbl_categories
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_categories" ADD CONSTRAINT "rsf_pfcbl_categories_pfcbl_category_pfcbl_rank_key" UNIQUE ("pfcbl_category", "pfcbl_rank");

-- ----------------------------
-- Primary Key structure for table rsf_pfcbl_categories
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_categories" ADD CONSTRAINT "rsf_pfcbl_categories_pkey" PRIMARY KEY ("pfcbl_category");

-- ----------------------------
-- Indexes structure for table rsf_pfcbl_ids
-- ----------------------------
CREATE INDEX "rsf_pfcbl_ids_created_by_reporting_cohort_id_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "created_by_reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_pfcbl_category_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "pfcbl_category" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_pfcbl_category_rank_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "pfcbl_category_rank" "pg_catalog"."int2_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_rsf_borrower_id_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_borrower_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_rsf_client_id_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_client_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_rsf_facility_id_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_facility_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_rsf_loan_id_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_loan_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_rsf_program_id_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_pfcbl_ids
-- ----------------------------
CREATE TRIGGER "trigger_delete_rsf_id" AFTER DELETE ON "p_rsf"."rsf_pfcbl_ids"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."delete_rsf_id"();
CREATE TRIGGER "trigger_insert_rsf_pfcbl_id_2_lcu" AFTER INSERT ON "p_rsf"."rsf_pfcbl_ids"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."insert_rsf_pfcbl_id_lcu"();
CREATE TRIGGER "trigger_insert_rsf_pfcbl_id_3_data" AFTER INSERT ON "p_rsf"."rsf_pfcbl_ids"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."insert_rsf_pfcbl_id_data"();
CREATE TRIGGER "trigger_insert_rsf_pfcbl_id_4_evaluations" AFTER INSERT ON "p_rsf"."rsf_pfcbl_ids"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."insert_rsf_pfcbl_id_evaluations"();

-- ----------------------------
-- Uniques structure for table rsf_pfcbl_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids_rsf_pfcbl_id_pfcbl_category_key" UNIQUE ("rsf_pfcbl_id", "pfcbl_category");

-- ----------------------------
-- Checks structure for table rsf_pfcbl_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "child_pfcbl_categories_have_parent_rsf_ids" CHECK (
CASE
    WHEN pfcbl_category::text = 'loan'::text THEN rsf_loan_id IS NOT NULL AND rsf_borrower_id IS NOT NULL
    WHEN pfcbl_category::text = 'borrower'::text THEN rsf_borrower_id IS NOT NULL AND rsf_client_id IS NOT NULL
    WHEN pfcbl_category::text = 'client'::text THEN rsf_client_id IS NOT NULL AND rsf_facility_id IS NOT NULL
    WHEN pfcbl_category::text = 'facility'::text THEN rsf_facility_id IS NOT NULL AND rsf_program_id IS NOT NULL
    ELSE true
END);
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "global_ids_are_zero" CHECK (
CASE
    WHEN rsf_program_id = 0 THEN rsf_pfcbl_id = 0
    ELSE rsf_pfcbl_id > 0
END);
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "global_program_id_is_zero" CHECK (
CASE
    WHEN pfcbl_category::text = 'global'::text THEN rsf_program_id = 0
    ELSE rsf_program_id > 0
END);

-- ----------------------------
-- Primary Key structure for table rsf_pfcbl_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids_pkey" PRIMARY KEY ("rsf_pfcbl_id");

-- ----------------------------
-- Cluster option for table rsf_pfcbl_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" CLUSTER ON "rsf_pfcbl_ids_pkey";

-- ----------------------------
-- Indexes structure for table rsf_pfcbl_reporting
-- ----------------------------
CREATE UNIQUE INDEX "rsf_pfcbl_reporting-entity_indicator_date-udx" ON "p_rsf"."rsf_pfcbl_reporting" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_reporting_created_by_data_id_idx" ON "p_rsf"."rsf_pfcbl_reporting" USING btree (
  "created_by_data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_reporting_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_pfcbl_reporting" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table rsf_pfcbl_reporting
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_reporting" ADD CONSTRAINT "rsf_pfcbl_reporting_pkey" PRIMARY KEY ("rsf_pfcbl_id", "reporting_asof_date");

-- ----------------------------
-- Indexes structure for table rsf_pfcbl_reporting_template_row_ids
-- ----------------------------
CREATE INDEX "rsf_pfcbl_reporting_template_row_ids_parent_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_pfcbl_reporting_template_row_ids" USING btree (
  "parent_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_reporting_template_row_ids_reporting_cohort_id_idx" ON "p_rsf"."rsf_pfcbl_reporting_template_row_ids" USING btree (
  "import_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_reporting_template_row_ids_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_pfcbl_reporting_template_row_ids" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Uniques structure for table rsf_pfcbl_reporting_template_row_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_reporting_template_row_ids" ADD CONSTRAINT "unique_row_id_per_entity_per_template" UNIQUE ("rsf_pfcbl_id", "data_source_row_id", "template_id");

-- ----------------------------
-- Primary Key structure for table rsf_pfcbl_reporting_template_row_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_reporting_template_row_ids" ADD CONSTRAINT "rsf_pfcbl_reporting_template_row_ids_pkey" PRIMARY KEY ("rsf_pfcbl_id", "data_source_row_id", "template_id");

-- ----------------------------
-- Indexes structure for table rsf_program_facility_check_guidance
-- ----------------------------
CREATE UNIQUE INDEX "rsf_program_facility_check_gu_indicator_check_guidance_id_r_idx" ON "p_rsf"."rsf_program_facility_check_guidance" USING btree (
  "indicator_check_guidance_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "rsf_facility_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_program_facility_check_guidance
-- ----------------------------
CREATE TRIGGER "trigger_check_valid_guidance_entry" BEFORE INSERT ON "p_rsf"."rsf_program_facility_check_guidance"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."check_valid_guidance_entry"();

-- ----------------------------
-- Primary Key structure for table rsf_program_facility_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_program_facility_check_guidance" ADD CONSTRAINT "rsf_program_check_guidance_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_check_guidance_id");

-- ----------------------------
-- Indexes structure for table rsf_program_facility_template_headers
-- ----------------------------
CREATE INDEX "rsf_program_facility_template_rsf_pfcbl_id_template_id_temp_idx" ON "p_rsf"."rsf_program_facility_template_headers" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "template_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "template_header_full_normalized" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_program_facility_templates_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_program_facility_template_headers" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_program_facility_templates_template_id_idx" ON "p_rsf"."rsf_program_facility_template_headers" USING btree (
  "template_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_program_facility_template_headers
-- ----------------------------
CREATE TRIGGER "trigger_changed_rsf_program_facility_template_headers" BEFORE INSERT OR UPDATE ON "p_rsf"."rsf_program_facility_template_headers"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_program_facility_template_headers_normalized"();

-- ----------------------------
-- Uniques structure for table rsf_program_facility_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "unique_entity_template_header_action_mapping" UNIQUE ("rsf_pfcbl_id", "template_id", "template_header_full_normalized", "action_mapping");

-- ----------------------------
-- Checks structure for table rsf_program_facility_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "valid_mappings" CHECK (true OR
CASE
    WHEN action = ANY (ARRAY['default'::text, 'ignore'::text, 'parse'::text]) THEN map_indicator_id IS NULL AND map_formula_id IS NULL AND map_check_formula_id IS NULL
    WHEN action = ANY (ARRAY['remap'::text, 'unmap'::text]) THEN map_indicator_id IS NOT NULL AND map_formula_id IS NULL AND map_check_formula_id IS NULL
    WHEN action = 'calculate'::text THEN map_indicator_id IS NULL AND map_formula_id IS NOT NULL AND map_check_formula_id IS NULL
    WHEN action = 'check'::text THEN map_indicator_id IS NULL AND map_formula_id IS NULL AND map_check_formula_id IS NOT NULL
    ELSE true
END);
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "valid_actions" CHECK (action = ANY (ARRAY['default'::text, 'ignore'::text, 'remap'::text, 'unmap'::text, 'check'::text, 'calculate'::text, 'parse'::text]));
COMMENT ON CONSTRAINT "valid_mappings" ON "p_rsf"."rsf_program_facility_template_headers" IS 'Not used because UI requires drop down select and then saves selection before mapping selection can be made';
COMMENT ON CONSTRAINT "valid_actions" ON "p_rsf"."rsf_program_facility_template_headers" IS 'Will allow duplicates when mappings are all NULL';

-- ----------------------------
-- Primary Key structure for table rsf_program_facility_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "rsf_program_facility_templates_pkey" PRIMARY KEY ("header_id");

-- ----------------------------
-- Primary Key structure for table rsf_program_settings
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_program_settings" ADD CONSTRAINT "rsf_program_settings_pkey" PRIMARY KEY ("rsf_program_id", "setting_name");

-- ----------------------------
-- Checks structure for table rsf_programs
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_programs" ADD CONSTRAINT "programs_can_report_quarterly_or_monthly" CHECK (ARRAY[reporting_period::text] && ARRAY['quarter'::text]);
COMMENT ON CONSTRAINT "programs_can_report_quarterly_or_monthly" ON "p_rsf"."rsf_programs" IS 'In theory, different programs might have monthly, etc reporting obligations and so this was designed to accommodate.  In practice, RSFs report quarterly and this assumption is baked-in so much that this field isn''t super meaningful.';

-- ----------------------------
-- Primary Key structure for table rsf_programs
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_programs" ADD CONSTRAINT "rsf_programs_pkey" PRIMARY KEY ("rsf_program_id");

-- ----------------------------
-- Indexes structure for table rsf_setup_checks
-- ----------------------------
CREATE INDEX "rsf_setup_checks-check_formula_id_idx" ON "p_rsf"."rsf_setup_checks" USING btree (
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_setup_checks-rsf_pfcbl_id_idx" ON "p_rsf"."rsf_setup_checks" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_setup_checks
-- ----------------------------
CREATE TRIGGER "trigger_rsf_setup_checks_1_subscription_allowed" BEFORE INSERT ON "p_rsf"."rsf_setup_checks"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_setup_checks_subscription_allowed"();
CREATE TRIGGER "trigger_rsf_setup_checks_2_auto_monitor_parameters_insert" AFTER INSERT ON "p_rsf"."rsf_setup_checks"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_checks_auto_monitor_parameters"();
CREATE TRIGGER "trigger_rsf_setup_checks_2_auto_monitor_parameters_update" AFTER UPDATE ON "p_rsf"."rsf_setup_checks"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_checks_auto_monitor_parameters"();

-- ----------------------------
-- Primary Key structure for table rsf_setup_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "rsf_setup_checks_pkey" PRIMARY KEY ("rsf_pfcbl_id", "check_formula_id");

-- ----------------------------
-- Indexes structure for table rsf_setup_checks_config
-- ----------------------------
CREATE INDEX "rsf_setup_checks_config_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_setup_checks_config" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Checks structure for table rsf_setup_checks_config
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "validate_config_check_class" CHECK (config_check_class = ANY (ARRAY['info'::text, 'warning'::text, 'error'::text, 'critical'::text]));

-- ----------------------------
-- Primary Key structure for table rsf_setup_checks_config
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config_pkey" PRIMARY KEY ("rsf_pfcbl_id", "for_indicator_id", "indicator_check_id");

-- ----------------------------
-- Indexes structure for table rsf_setup_indicators
-- ----------------------------
CREATE INDEX "rsf_setup_indicators-formula_id" ON "p_rsf"."rsf_setup_indicators" USING btree (
  "formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_setup_indicators-indicator_id" ON "p_rsf"."rsf_setup_indicators" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_setup_indicators-rsf_pfcbl_id" ON "p_rsf"."rsf_setup_indicators" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "rsf_setup_indicators-rsf_pfcbl_id_formula_id" ON "p_rsf"."rsf_setup_indicators" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "rsf_setup_indicators-rsf_pfcbl_id_indicator_id" ON "p_rsf"."rsf_setup_indicators" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_setup_indicators
-- ----------------------------
CREATE TRIGGER "trigger_rsf_setup_indicators_1_subscription_allowed" BEFORE INSERT ON "p_rsf"."rsf_setup_indicators"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_subscription_allowed"();
CREATE TRIGGER "trigger_rsf_setup_indicators_2_subscription_calculation_unit" BEFORE INSERT OR UPDATE ON "p_rsf"."rsf_setup_indicators"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_validate_calculation_unit"();
CREATE TRIGGER "trigger_rsf_setup_indicators_3_auto_monitor_parameters_insert" AFTER INSERT ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_auto_monitor_parameters"();
CREATE TRIGGER "trigger_rsf_setup_indicators_3_auto_monitor_parameters_update" AFTER UPDATE ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_auto_monitor_parameters"();
CREATE TRIGGER "trigger_rsf_setup_indicators_4_auto_monitor_checks_delete" AFTER DELETE ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_auto_monitor_checks"();
CREATE TRIGGER "trigger_rsf_setup_indicators_4_auto_monitor_checks_insert" AFTER INSERT ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_auto_monitor_checks"();
CREATE TRIGGER "trigger_rsf_setup_indicators_4_auto_monitor_checks_update" AFTER UPDATE ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_auto_monitor_checks"();
CREATE TRIGGER "trigger_rsf_setup_indicators_5_delete_calculated_data_delete" AFTER DELETE ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_delete_calculated_data"();
CREATE TRIGGER "trigger_rsf_setup_indicators_5_delete_calculated_data_update" AFTER UPDATE ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_delete_calculated_data"();
CREATE TRIGGER "trigger_rsf_setup_indicators_6_subscription_recalculations_inse" AFTER INSERT ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_subscription_recalculations"();
CREATE TRIGGER "trigger_rsf_setup_indicators_6_subscription_recalculations_upda" AFTER UPDATE ON "p_rsf"."rsf_setup_indicators"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_setup_indicators_subscription_recalculations"();

-- ----------------------------
-- Checks structure for table rsf_setup_indicators
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "valid_calculation_units" CHECK (formula_calculation_unit IS NULL OR formula_calculation_unit ~ '[A-Z]{3}'::text);

-- ----------------------------
-- Primary Key structure for table rsf_setup_indicators
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_id");

-- ----------------------------
-- Foreign Keys structure for table !dep-rsf_pfcbl_id_family
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_pfcbl_id_family" ADD CONSTRAINT "rsf_pfcbl_id_family_child_rsf_pfcbl_id_fkey" FOREIGN KEY ("child_rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."!dep-rsf_pfcbl_id_family" ADD CONSTRAINT "rsf_pfcbl_id_family_parent_rsf_pfcbl_id_fkey" FOREIGN KEY ("parent_rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION;

-- ----------------------------
-- Foreign Keys structure for table !dep-rsf_program_reporting_dates
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_program_reporting_dates" ADD CONSTRAINT "rsf_program_reporting_dates_rsf_program_id_fkey" FOREIGN KEY ("rsf_program_id") REFERENCES "p_rsf"."rsf_programs" ("rsf_program_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table export_template_reports
-- ----------------------------
ALTER TABLE "p_rsf"."export_template_reports" ADD CONSTRAINT "export_template_reports_export_template_id_fkey" FOREIGN KEY ("export_template_id") REFERENCES "p_rsf"."export_templates" ("export_template_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."export_template_reports" ADD CONSTRAINT "export_template_reports_report_id_fkey" FOREIGN KEY ("report_id") REFERENCES "p_rsf"."reports" ("report_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table exporting_cohorts
-- ----------------------------
ALTER TABLE "p_rsf"."exporting_cohorts" ADD CONSTRAINT "exporting_cohorts_exporting_rsf_pfcbl_id_fkey" FOREIGN KEY ("exporting_rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table indicator_check_formula_parameters
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_formula_parameters" ADD CONSTRAINT "indicator_check_formula_parameter_paraemter_pfcbl_category_fkey" FOREIGN KEY ("parameter_pfcbl_category", "parameter_pfcbl_rank") REFERENCES "p_rsf"."rsf_pfcbl_categories" ("pfcbl_category", "pfcbl_rank") ON DELETE RESTRICT ON UPDATE RESTRICT;
ALTER TABLE "p_rsf"."indicator_check_formula_parameters" ADD CONSTRAINT "indicator_check_formula_parameters-check_formula_id-fx" FOREIGN KEY ("check_formula_id", "for_pfcbl_category") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id", "check_pfcbl_category") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicator_check_formula_parameters" ADD CONSTRAINT "indicator_check_formula_parameters-indicator_check_id-fkey" FOREIGN KEY ("check_formula_id", "indicator_check_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id", "indicator_check_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicator_check_formula_parameters" ADD CONSTRAINT "indicator_check_formula_parameters_parameter_indicator_id_fkey" FOREIGN KEY ("parameter_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table indicator_check_formulas
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_formulas" ADD CONSTRAINT "indicator_check_formulas_indicator_check_id_fkey" FOREIGN KEY ("indicator_check_id", "check_pfcbl_category") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id", "check_pfcbl_category") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table indicator_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_check_guidance" ADD CONSTRAINT "indicator_check_guidance_for_indicator_id_fkey" FOREIGN KEY ("for_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicator_check_guidance" ADD CONSTRAINT "indicator_check_guidance_indicator_check_id_fkey" FOREIGN KEY ("indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table indicator_checks
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "indicator_checks_check_pfcbl_category_fkey" FOREIGN KEY ("check_pfcbl_category") REFERENCES "p_rsf"."rsf_pfcbl_categories" ("pfcbl_category") ON DELETE NO ACTION ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "indicator_checks_check_type_fkey" FOREIGN KEY ("check_type") REFERENCES "p_rsf"."indicator_check_types" ("check_type") ON DELETE NO ACTION ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "indicator_checks_grouping_fkey" FOREIGN KEY ("grouping") REFERENCES "p_rsf"."rsf_pfcbl_categories" ("pfcbl_category") ON DELETE NO ACTION ON UPDATE NO ACTION;

-- ----------------------------
-- Foreign Keys structure for table indicator_formula_parameters
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_formula_parameters" ADD CONSTRAINT "indicator_formula_parameters_formula_id_fkey" FOREIGN KEY ("formula_id", "indicator_id") REFERENCES "p_rsf"."indicator_formulas" ("formula_id", "indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicator_formula_parameters" ADD CONSTRAINT "indicator_formula_parameters_indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicator_formula_parameters" ADD CONSTRAINT "indicator_formula_parameters_parameter_indicator_id_fkey" FOREIGN KEY ("parameter_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table indicator_formulas
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "indicator_formulas_formula_calculated_by_indicator_id_fkey" FOREIGN KEY ("dep-formula_calculated_by_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "indicator_formulas_indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table indicator_options_group_keys
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_options_group_keys" ADD CONSTRAINT "indicator_options_group_keys_label_id_fkey" FOREIGN KEY ("label_id") REFERENCES "p_rsf"."label_ids" ("label_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicator_options_group_keys" ADD CONSTRAINT "indicator_options_group_keys_options_group_id_fkey" FOREIGN KEY ("options_group_id") REFERENCES "p_rsf"."indicator_options_groups" ("options_group_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table indicators
-- ----------------------------
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_data_category_fkey" FOREIGN KEY ("data_category") REFERENCES "p_rsf"."rsf_pfcbl_categories" ("pfcbl_category") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_data_type_fkey" FOREIGN KEY ("data_type") REFERENCES "p_rsf"."indicator_data_types" ("data_type") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_indicator_id_category_fkey" FOREIGN KEY ("indicator_sys_category") REFERENCES "p_rsf"."indicator_sys_categories" ("indicator_sys_category") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_indicator_options_group_id_fkey" FOREIGN KEY ("indicator_options_group_id") REFERENCES "p_rsf"."indicator_options_groups" ("options_group_id") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_label_id_fkey" FOREIGN KEY ("label_id") REFERENCES "p_rsf"."label_ids" ("label_id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table label_keys
-- ----------------------------
ALTER TABLE "p_rsf"."label_keys" ADD CONSTRAINT "label_keys_key_type_template_id_fkey" FOREIGN KEY ("key_type_template_id") REFERENCES "p_rsf"."reporting_templates" ("template_id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table labels
-- ----------------------------
ALTER TABLE "p_rsf"."labels" ADD CONSTRAINT "labels_label_id_fkey" FOREIGN KEY ("label_id") REFERENCES "p_rsf"."label_ids" ("label_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."labels" ADD CONSTRAINT "labels_label_key_fkey" FOREIGN KEY ("label_key") REFERENCES "p_rsf"."label_keys" ("label_key") ON DELETE RESTRICT ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table reporting_cohort_info
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_cohort_info" ADD CONSTRAINT "reporting_cohort_info-reporting_cohort_id_fkey" FOREIGN KEY ("reporting_cohort_id") REFERENCES "p_rsf"."reporting_cohorts" ("reporting_cohort_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table reporting_cohorts
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "reporting_cohorts-import_id_fkey" FOREIGN KEY ("import_id") REFERENCES "p_rsf"."reporting_imports" ("import_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "reporting_cohorts-reporting_rsf_pfcbl_id_fkey" FOREIGN KEY ("reporting_rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table reporting_import_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_import_template_headers" ADD CONSTRAINT "reporting_import_template_headers_indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."reporting_import_template_headers" ADD CONSTRAINT "reporting_import_template_headers_reporting_cohort_id_fkey" FOREIGN KEY ("import_id") REFERENCES "p_rsf"."reporting_imports" ("import_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."reporting_import_template_headers" ADD CONSTRAINT "reporting_import_template_headers_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_clients
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_clients" ADD CONSTRAINT "rsf_clients_rsf_facility_id_fkey" FOREIGN KEY ("rsf_facility_id") REFERENCES "p_rsf"."rsf_facilities" ("rsf_facility_id") ON DELETE NO ACTION ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_data
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data" ADD CONSTRAINT "rsf_data-indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE NO ACTION ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_data" ADD CONSTRAINT "rsf_data-reporting_cohort_id_fkey" FOREIGN KEY ("reporting_cohort_id") REFERENCES "p_rsf"."reporting_cohorts" ("reporting_cohort_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data" ADD CONSTRAINT "rsf_data-rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_calculation_validations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_calculation_validations" ADD CONSTRAINT "rsf_data_calculation_validations_data_id_fkey" FOREIGN KEY ("data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE;
ALTER TABLE "p_rsf"."rsf_data_calculation_validations" ADD CONSTRAINT "rsf_data_calculation_validations_indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_calculation_validations" ADD CONSTRAINT "rsf_data_calculation_validations_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks-formula_null_or_exists" FOREIGN KEY ("check_formula_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id") ON DELETE CASCADE ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_consolidated_from_indicator_check_id_fkey" FOREIGN KEY ("consolidated_from_indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE CASCADE ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_data_id_fkey" FOREIGN KEY ("data_id") REFERENCES "p_rsf"."rsf_data" ("data_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_indicator_check_guidance_id_fkey" FOREIGN KEY ("indicator_check_guidance_id") REFERENCES "p_rsf"."indicator_check_guidance" ("indicator_check_guidance_id") ON DELETE SET NULL ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_indicator_check_id_fkey" FOREIGN KEY ("indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE CASCADE ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_rsf_pfcbl_id_check_asof_date_fkey" FOREIGN KEY ("rsf_pfcbl_id", "check_asof_date") REFERENCES "p_rsf"."rsf_pfcbl_reporting" ("rsf_pfcbl_id", "reporting_asof_date") ON DELETE CASCADE ON UPDATE NO ACTION;
COMMENT ON CONSTRAINT "rsf_data_checks_data_id_fkey" ON "p_rsf"."rsf_data_checks" IS 'RSF_DATA as non-current data points may have had checks that have actions set or notes';

-- ----------------------------
-- Foreign Keys structure for table rsf_data_checks_archive
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_check_formula_id_fkey" FOREIGN KEY ("check_formula_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_consolidated_from_indicator_check__f" FOREIGN KEY ("consolidated_from_indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_consolidated_from_indicator_id_fkey" FOREIGN KEY ("consolidated_from_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_indicator_check_id_fkey" FOREIGN KEY ("indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE SET NULL ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_current
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current" ADD CONSTRAINT "rsf_data_current-data_id_IN_rsf_data" FOREIGN KEY ("data_id") REFERENCES "p_rsf"."rsf_data" ("data_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_current" ADD CONSTRAINT "rsf_data_current-data_unit_data_id_IN_rsf_data_current" FOREIGN KEY ("data_unit_data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_current_fx
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_fx" ADD CONSTRAINT "rsf_data_current_fx_fx_data_id_fkey" FOREIGN KEY ("fx_data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_current_fx" ADD CONSTRAINT "rsf_data_current_fx_rsf_pfcbl_id_reporting_asof_date_fkey" FOREIGN KEY ("rsf_pfcbl_id", "reporting_asof_date") REFERENCES "p_rsf"."rsf_pfcbl_reporting" ("rsf_pfcbl_id", "reporting_asof_date") ON DELETE CASCADE ON UPDATE NO ACTION;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_current_lcu
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_lcu" ADD CONSTRAINT "rsf_data_current_lcu_for_rsf_pfcbl_id_fkey" FOREIGN KEY ("for_rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_current_lcu" ADD CONSTRAINT "rsf_data_current_lcu_lcu_unit_data_id_fkey" FOREIGN KEY ("lcu_unit_data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
COMMENT ON CONSTRAINT "rsf_data_current_lcu_lcu_unit_data_id_fkey" ON "p_rsf"."rsf_data_current_lcu" IS 'Enables historic updates to cascade into the future';

-- ----------------------------
-- Foreign Keys structure for table rsf_data_current_names_and_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_names_and_ids" ADD CONSTRAINT "rsf_data_current_names_and_id_rsf_pfcbl_id_reporting_asof__fkey" FOREIGN KEY ("rsf_pfcbl_id", "reporting_asof_date") REFERENCES "p_rsf"."rsf_pfcbl_reporting" ("rsf_pfcbl_id", "reporting_asof_date") ON DELETE CASCADE ON UPDATE NO ACTION;

-- ----------------------------
-- Foreign Keys structure for table rsf_facilities
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_facilities" ADD CONSTRAINT "rsf_facilities_rsf_program_id_fkey" FOREIGN KEY ("rsf_program_id") REFERENCES "p_rsf"."rsf_programs" ("rsf_program_id") ON DELETE NO ACTION ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_loan_issuance_series
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_loan_issuance_series" ADD CONSTRAINT "rsf_loan_issuance_series_id_value_data_id_fkey" FOREIGN KEY ("id_value_data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE;
ALTER TABLE "p_rsf"."rsf_loan_issuance_series" ADD CONSTRAINT "rsf_loan_issuance_series_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION;

-- ----------------------------
-- Foreign Keys structure for table rsf_pfcbl_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-borrower_has_client" FOREIGN KEY ("rsf_client_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE RESTRICT;
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-client_has_facility" FOREIGN KEY ("rsf_facility_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE RESTRICT;
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-created_by_reporting_cohort_id_fkey" FOREIGN KEY ("created_by_reporting_cohort_id") REFERENCES "p_rsf"."reporting_cohorts" ("reporting_cohort_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-facility_has_program" FOREIGN KEY ("rsf_program_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE RESTRICT;
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-loan_has_borrower" FOREIGN KEY ("rsf_borrower_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE RESTRICT;
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-pfcbl_category_fkey" FOREIGN KEY ("pfcbl_category", "pfcbl_category_rank") REFERENCES "p_rsf"."rsf_pfcbl_categories" ("pfcbl_category", "pfcbl_rank") ON DELETE RESTRICT ON UPDATE RESTRICT;
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-rsf_client_id_fkey" FOREIGN KEY ("rsf_client_id") REFERENCES "p_rsf"."rsf_clients" ("rsf_client_id") ON DELETE NO ACTION ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-rsf_facility_id_fkey" FOREIGN KEY ("rsf_facility_id") REFERENCES "p_rsf"."rsf_facilities" ("rsf_facility_id") ON DELETE NO ACTION ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_pfcbl_ids" ADD CONSTRAINT "rsf_pfcbl_ids-rsf_program_id_fkey" FOREIGN KEY ("rsf_program_id") REFERENCES "p_rsf"."rsf_programs" ("rsf_program_id") ON DELETE NO ACTION ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;
COMMENT ON CONSTRAINT "rsf_pfcbl_ids-created_by_reporting_cohort_id_fkey" ON "p_rsf"."rsf_pfcbl_ids" IS 'Deferrable, initially deferred to enable creating new programs before the reporting_cohort needed to report their creation.  This is a fundamental constraint since reporting_cohorts are the source of deletes';
COMMENT ON CONSTRAINT "rsf_pfcbl_ids-rsf_program_id_fkey" ON "p_rsf"."rsf_pfcbl_ids" IS 'This will cause the trigger_remove_pfcbl_id trigger to fire';

-- ----------------------------
-- Foreign Keys structure for table rsf_pfcbl_reporting
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_reporting" ADD CONSTRAINT "rsf_pfcbl_reporting-creatd_by_data_id_fkey" FOREIGN KEY ("created_by_data_id") REFERENCES "p_rsf"."rsf_data" ("data_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_pfcbl_reporting" ADD CONSTRAINT "rsf_pfcbl_reporting-rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_pfcbl_reporting_template_row_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_pfcbl_reporting_template_row_ids" ADD CONSTRAINT "rsf_pfcbl_reporting_template_row_ids_import_id_fkey" FOREIGN KEY ("import_id") REFERENCES "p_rsf"."reporting_imports" ("import_id") ON DELETE CASCADE ON UPDATE NO ACTION;

-- ----------------------------
-- Foreign Keys structure for table rsf_program_facility_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_program_facility_check_guidance" ADD CONSTRAINT "rsf_program_facility_check_gui_indicator_check_guidance_id_fkey" FOREIGN KEY ("indicator_check_guidance_id") REFERENCES "p_rsf"."indicator_check_guidance" ("indicator_check_guidance_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_program_facility_check_guidance" ADD CONSTRAINT "rsf_program_facility_check_guidance_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_program_facility_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "rsf_program_facility_template_headers_map_calculation_formula" FOREIGN KEY ("map_formula_id") REFERENCES "p_rsf"."indicator_formulas" ("formula_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "rsf_program_facility_template_headers_map_check_formula" FOREIGN KEY ("map_check_formula_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "rsf_program_facility_template_headers_map_indicator_id" FOREIGN KEY ("map_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "rsf_program_facility_templates_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_program_facility_template_headers" ADD CONSTRAINT "rsf_program_facility_templates_template_id_fkey" FOREIGN KEY ("template_id") REFERENCES "p_rsf"."reporting_templates" ("template_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_program_settings
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_program_settings" ADD CONSTRAINT "rsf_program_settings_rsf_program_id_fkey" FOREIGN KEY ("rsf_program_id") REFERENCES "p_rsf"."rsf_programs" ("rsf_program_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_program_settings" ADD CONSTRAINT "rsf_program_settings_setting_name_fkey" FOREIGN KEY ("setting_name") REFERENCES "p_rsf"."program_settings" ("setting_name") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_setup_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "rsf_setup_checks-cohort_id" FOREIGN KEY ("auto_subscribed_by_reporting_cohort_id") REFERENCES "p_rsf"."reporting_cohorts" ("reporting_cohort_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "rsf_setup_checks_check_formula_id_fkey" FOREIGN KEY ("check_formula_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "rsf_setup_checks_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_setup_checks_config
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config-check_id_fkey" FOREIGN KEY ("indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config-indicator_id_fkey" FOREIGN KEY ("for_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config-rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_setup_indicators
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_formula_id_fkey" FOREIGN KEY ("formula_id") REFERENCES "p_rsf"."indicator_formulas" ("formula_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;
