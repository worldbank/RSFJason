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

 Date: 28/07/2026 22:40:19
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
-- Sequence structure for indicator_classifications_classification_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."indicator_classifications_classification_id_seq";
CREATE SEQUENCE "p_rsf"."indicator_classifications_classification_id_seq" 
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
-- Sequence structure for rsf_settings_archive_archive_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."rsf_settings_archive_archive_id_seq";
CREATE SEQUENCE "p_rsf"."rsf_settings_archive_archive_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Sequence structure for rsf_setup_checks_config_config_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "p_rsf"."rsf_setup_checks_config_config_id_seq";
CREATE SEQUENCE "p_rsf"."rsf_setup_checks_config_config_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 2147483647
START 1
CACHE 1;

-- ----------------------------
-- Table structure for !dep-indicator_check_guidance
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."!dep-indicator_check_guidance";
CREATE TABLE "p_rsf"."!dep-indicator_check_guidance" (
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
COMMENT ON COLUMN "p_rsf"."!dep-indicator_check_guidance"."is_resolving_guidance" IS 'When set to true, enables users to refine check behavior to auto-resolve for any combination of check_ids and indicator_ids, using the specified guidance as the auto-resolving resolution comment and user_id as the auto-resolving user';
COMMENT ON COLUMN "p_rsf"."!dep-indicator_check_guidance"."variance_threshold" IS 'For checks that have a variance threshold, if the variance is within the specified threshold then this guidance will be aplied';
COMMENT ON COLUMN "p_rsf"."!dep-indicator_check_guidance"."check_formula_id" IS 'TBD: Do we care more about the application or the calculation; or both?';

-- ----------------------------
-- Table structure for !dep-reporting_cohort_info
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."!dep-reporting_cohort_info";
CREATE TABLE "p_rsf"."!dep-reporting_cohort_info" (
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
-- Table structure for !dep-rsf_pfcbl_reporting
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."!dep-rsf_pfcbl_reporting";
CREATE TABLE "p_rsf"."!dep-rsf_pfcbl_reporting" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "reporting_asof_date" date NOT NULL,
  "created_by_data_id" int4 NOT NULL,
  "reporting_indicator_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for !dep-rsf_program_facility_check_guidance
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."!dep-rsf_program_facility_check_guidance";
CREATE TABLE "p_rsf"."!dep-rsf_program_facility_check_guidance" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "indicator_check_guidance_id" int4 NOT NULL,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "applied_by_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "application_time" timestamptz(6) NOT NULL
)
;
COMMENT ON COLUMN "p_rsf"."!dep-rsf_program_facility_check_guidance"."rsf_facility_id" IS 'Null when guidance is set at the program level';

-- ----------------------------
-- Table structure for !dep-rsf_program_settings
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."!dep-rsf_program_settings";
CREATE TABLE "p_rsf"."!dep-rsf_program_settings" (
  "rsf_program_id" int4 NOT NULL,
  "setting_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "setting_value" text COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for dashboard_exports
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."dashboard_exports";
CREATE TABLE "p_rsf"."dashboard_exports" (
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
  "exporting_asof_date" date,
  "dashboard_parameters" jsonb,
  "exporting_title" text COLLATE "pg_catalog"."default"
)
;
COMMENT ON COLUMN "p_rsf"."dashboard_exports"."exporting_rsf_pfcbl_id" IS 'parent rsf reporting ID';

-- ----------------------------
-- Table structure for dashboard_reports
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."dashboard_reports";
CREATE TABLE "p_rsf"."dashboard_reports" (
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
  "computation_group" int4 NOT NULL DEFAULT 0,
  "unit_fx_method" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'calculation'::text,
  "check_formula_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_check_formulas_check_formula_id_seq'::regclass),
  "check_formula_title" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'Untitled check formula'::text,
  "default_subscription" bool NOT NULL DEFAULT false,
  "variance_formula" text COLLATE "pg_catalog"."default"
)
;
COMMENT ON COLUMN "p_rsf"."indicator_check_formulas"."parameter_pfcbl_ranks" IS 'Includes parameter ranks, only';
COMMENT ON COLUMN "p_rsf"."indicator_check_formulas"."parent_grouping_pfcbl_rank" IS 'For grouping and/or child-level parameters';
COMMENT ON COLUMN "p_rsf"."indicator_check_formulas"."variance_formula" IS 'For custom variance calculations';

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
  "is_data_check" bool NOT NULL DEFAULT false,
  "modification_time" timestamp(6) NOT NULL DEFAULT now(),
  "variance_tolerance_allowed" bool NOT NULL DEFAULT false,
  "is_calculator_check" bool NOT NULL DEFAULT false,
  "check_type" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'none'::text,
  "check_pfcbl_category" text COLLATE "pg_catalog"."default",
  "auto_resolve_system_check" bool,
  "auto_subscribe" bool DEFAULT true,
  "data_sys_flags_granted" int4 NOT NULL DEFAULT 0
)
;
COMMENT ON COLUMN "p_rsf"."indicator_checks"."variance_tolerance_allowed" IS 'When true, indicates a % variance from an existing value and if outside that tolerance range, will apply the flag; and if not, flag ignored.  Only relevant for system checks, notably system calculator overwrites or other "disagreement" type flags.  Enabled through custom guidance application';
COMMENT ON COLUMN "p_rsf"."indicator_checks"."check_pfcbl_category" IS 'Null means is_system=true since a single system flag can be applied on any data point';
COMMENT ON COLUMN "p_rsf"."indicator_checks"."data_sys_flags_granted" IS 'data_sys_flags that are "granted" to this check (user may apply it due to check''s nature rather than check''s data context)';

-- ----------------------------
-- Table structure for indicator_classifications
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_classifications";
CREATE TABLE "p_rsf"."indicator_classifications" (
  "classification_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_classifications_classification_id_seq'::regclass),
  "classification" text COLLATE "pg_catalog"."default" NOT NULL,
  "notes" text COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for indicator_data_types
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."indicator_data_types";
CREATE TABLE "p_rsf"."indicator_data_types" (
  "data_type" text COLLATE "pg_catalog"."default" NOT NULL
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
  "formula_id" int4 NOT NULL,
  "calculate_pfcbl_rank" int2 NOT NULL
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
  "perform_calculation_by_row" bool DEFAULT false,
  "modification_time" timestamp(6) NOT NULL DEFAULT now(),
  "computation_group" int4 NOT NULL DEFAULT 1,
  "!dep-computation_priority_rank" int2 DEFAULT 0,
  "formula_id" int4 NOT NULL DEFAULT nextval('"p_rsf".indicator_formulas_formula_id_seq'::regclass),
  "formula_title" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'Untitled Formula'::text,
  "is_primary_default" bool NOT NULL DEFAULT true,
  "formula_notes" text COLLATE "pg_catalog"."default",
  "modified_by_user_id" text COLLATE "pg_catalog"."default",
  "has_timeseries_parameters" bool NOT NULL DEFAULT false,
  "has_reporting_parameters" bool NOT NULL DEFAULT false,
  "has_no_parameters" bool NOT NULL DEFAULT false
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
COMMENT ON COLUMN "p_rsf"."indicator_formulas"."!dep-computation_priority_rank" IS 'Deprecated: only used to sort currency_ratio priority and too complex for simple need; Zero is low priority.  Used to segment formula_calculation_ranks into sub-ranks for independent calculation.  Practically, is used to calculate currency_ratio calculations first since other subsequent calculations may rely on these (or not) in an unknowable way depending on whether any given formula must undertake an fx conversion due to its input data, as determined at the computation time.';

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
  "is_required" bool NOT NULL DEFAULT false,
  "is_setup" text COLLATE "pg_catalog"."default",
  "modification_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone,
  "version_number" int4 NOT NULL DEFAULT 1,
  "default_subscription" bool NOT NULL DEFAULT false,
  "is_periodic_or_flow_reporting" bool NOT NULL DEFAULT false,
  "classification" text COLLATE "pg_catalog"."default",
  "sort_preference" int2,
  "created_by_user_id" text COLLATE "pg_catalog"."default",
  "modified_by_user_id" text COLLATE "pg_catalog"."default",
  "pfcbl_rank" int2 NOT NULL,
  "unit_fx_method" text COLLATE "pg_catalog"."default" DEFAULT 'calculation'::text,
  "unit_fx_source" text COLLATE "pg_catalog"."default" DEFAULT 'default'::text,
  "unit_fx_indicator_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."indicators"."default_value" IS 'If an entity submits a column with an NA value, then use a default, if defined.  But if entity never submits any value (ever), then db_program_get_data will return NA';
COMMENT ON COLUMN "p_rsf"."indicators"."is_system" IS 'true when indicator is created within/by system processes, not externally defined';
COMMENT ON COLUMN "p_rsf"."indicators"."is_data_unit" IS 'If true, the data_value recorded in rsf_data is a unit of measure (which presumably defines the units of other data and is used in rsf_data_timeseries::data_unit_data_id)';
COMMENT ON COLUMN "p_rsf"."indicators"."is_required" IS 'If true calculated by the system independently AND uses the formula definition in indicator_formulas; if false, calcualted by the normal system calculator -- this is needed as queries to indicator formulas will pull these in and is_system_calculated informs the calculator to ignore these.  Note: that some system indicators are indeed calculated by the system, but do not have is_system_calculated=true because the are calculated entirely outside of any indicator_formula definition';
COMMENT ON COLUMN "p_rsf"."indicators"."is_setup" IS 'true when it''s a required field use to setup/initialize an entity; fields to present to UI';
COMMENT ON COLUMN "p_rsf"."indicators"."default_subscription" IS 'Application layer will submit first in a partial cohort to trigger rsf_data_timeseries to only manage this indicator data first (this is used by currency unit reporting to ensure that if updates to currency data units will exist before other data on the same timeline so rsf_data_timeseries interprets their LCU data units according to the current update in the same timeline)';
COMMENT ON COLUMN "p_rsf"."indicators"."is_periodic_or_flow_reporting" IS 'Overwhelmingly, data is STOCK data and static (or implicitly interpreted to be stock, such as a loan repayment can be interpreted as a single "last payment made" rather than a flow of payments over time).  This flags data that are flow: such as the QDD date or other data that are explicilty flow data associated with the reporting period.  

As of 2023, this  metric is NOT used by the calculator or database to determine changes.  Eg, if a flow indicator reports $100 this quarter and next quarter also reports $100, it will be discarded as a non-change.  Currently, as these indicators are rare it presents no issues.  This may be a todo item to update';
COMMENT ON COLUMN "p_rsf"."indicators"."unit_fx_method" IS 'if fx_indicator_id is a calculated indicator, then will replicate that indicator''s method';
COMMENT ON COLUMN "p_rsf"."indicators"."unit_fx_source" IS 'global, program or facility; or null default to closest available.  In practice, metric will define NULL (default) or global';
COMMENT ON COLUMN "p_rsf"."indicators"."unit_fx_indicator_id" IS 'This indicator becomes a "child" indicator governed by fx indicator_id';

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
-- Table structure for reporting_cohorts
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reporting_cohorts";
CREATE TABLE "p_rsf"."reporting_cohorts" (
  "reporting_cohort_id" int4 NOT NULL DEFAULT nextval('"p_rsf".rsf_data_cohort_sequence'::regclass),
  "reporting_asof_date" date NOT NULL,
  "reporting_rsf_pfcbl_id" int4 NOT NULL,
  "reporting_user_id" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'UNKNOWN'::text,
  "reporting_time" timestamptz(6) NOT NULL DEFAULT now(),
  "is_calculated_cohort" bool NOT NULL DEFAULT false,
  "is_reported_cohort" bool NOT NULL DEFAULT true,
  "data_asof_date" date NOT NULL,
  "import_id" int4 NOT NULL,
  "reporting_type" int2 NOT NULL DEFAULT 0,
  "reporting_calculation_rank" int2 NOT NULL DEFAULT 0
)
;
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."reporting_user_id" IS 'References: ARL.arlapplications.accounts.account_id';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."data_asof_date" IS 'If the data timestamp is not quarter end, precisely.';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."reporting_type" IS '0=Setup/System, 1=User Import, 2=Calculator';
COMMENT ON COLUMN "p_rsf"."reporting_cohorts"."reporting_calculation_rank" IS 'for calculated cohorts, to report the formula_calculation_rank of ALL data being reporting under this cohort EXCEPT for where is_reported_cohort is true and users have reported calculated data; but when system verifies data, each verification is inserted in tranches by calculation rank.

NOTE: that if a formula subscription changes in rsf_setup_indicators that retrospectively multiple formula_ids could become related to a reporting_cohort but it will be data for a single rank at the time of insert.  (this purpose is to help filtering calculation triggers by knowing that only current/subsequent ranks can be affected by the insert)';

-- ----------------------------
-- Table structure for reporting_dates
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."reporting_dates";
CREATE TABLE "p_rsf"."reporting_dates" (
  "quarter_end_date" date NOT NULL
)
;

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
  "is_complete_portfolio" bool NOT NULL DEFAULT false,
  "is_setup_template" bool NOT NULL DEFAULT false,
  "file_extension" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'xlsx'::text,
  "is_system" bool NOT NULL DEFAULT false,
  "is_zero_versionable" bool NOT NULL DEFAULT false
)
;
COMMENT ON COLUMN "p_rsf"."reporting_templates"."is_complete_portfolio" IS 'Template will reliably report the entire RSF portfolio each QR period (IFC client QRs do so to maintian internal metrics). And this is used to delete existing templates for same entity and reporting period, with the knowledge that any new template cannot be a partial set of portfolio data';
COMMENT ON COLUMN "p_rsf"."reporting_templates"."is_zero_versionable" IS 'If template allows "export to zero version feature"';

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
  "calculation_asof_date" date NOT NULL,
  "rsf_pf_id" int4 NOT NULL,
  "formula_calculation_rank" int2 NOT NULL
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
  "data_id" int4,
  "validation_time" timestamptz(6) NOT NULL DEFAULT (timeofday())::timestamp with time zone
)
;
COMMENT ON COLUMN "p_rsf"."rsf_data_calculation_validations"."data_id" IS 'If calculator failed, for example.  Rare case for a null data_id';

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
  "check_has_data" bool NOT NULL DEFAULT false,
  "check_data_id_is_current" bool NOT NULL,
  "check_formula_id" int4,
  "data_sys_flags" int2,
  "data_value_unit" text COLLATE "pg_catalog"."default",
  "archive_sys_name" text COLLATE "pg_catalog"."default",
  "data_correction_data_id" int4,
  "data_correction_date" date,
  "data_check_value" text COLLATE "pg_catalog"."default",
  "data_check_unit" text COLLATE "pg_catalog"."default",
  "for_import_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."check_has_data" IS 'Flag for alternative veiw of its data (has values for data_check_value and data_check_unit, which may be missing/NULL as valid data)';
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."data_sys_flags" IS 'Flags on rsf_data are set via flagging';
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."data_value_unit" IS 'Representation of the flagg data points concatenated data_value+data_unit used to un-archive flags against the same data_value_unit represented in the data at the same check_asof_date for the same indicator_id';
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."data_correction_date" IS 'Used by data_sys_flag to "repost" the flagged datapoint to an historical period for which the current data represents a correction.';
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."data_check_value" IS 'If the check has an alternative view of its data (eg, system calculator VS flag or reporting finalized flag), system will store the unporeted data here instead of rsf_data, as the data here has been disallowed from being reported for some reason';
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."data_check_unit" IS 'If the check has an alternative view of its data (eg, system calculator VS flag or reporting finalized flag), system will store the unporeted data here instead of rsf_data, as the data here has been disallowed from being reported for some reason';
COMMENT ON COLUMN "p_rsf"."rsf_data_checks"."for_import_id" IS 'For reporting checks (formatting, etc), will delete when import is deleted';

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
  "data_sys_flags" int2,
  "data_value_unit" text COLLATE "pg_catalog"."default",
  "evaluation_data" jsonb,
  "data_correction_date" date
)
;
COMMENT ON COLUMN "p_rsf"."rsf_data_checks_archive"."check_message" IS 'Used to restore checks to their original evaluation_id';
COMMENT ON COLUMN "p_rsf"."rsf_data_checks_archive"."data_value_unit" IS 'Used to restore flagged data';

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
  "data_unit_data_id" int4,
  "data_time" timestamptz(6) NOT NULL,
  "is_calculated" bool NOT NULL DEFAULT false,
  "is_periodic" bool NOT NULL DEFAULT false
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
  "series_id" text COLLATE "pg_catalog"."default",
  "data_cohort_id" int4 NOT NULL
)
;

-- ----------------------------
-- Table structure for rsf_data_sys_flags
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_data_sys_flags";
CREATE TABLE "p_rsf"."rsf_data_sys_flags" (
  "data_flag_value" int2 NOT NULL,
  "data_flag_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "comments" text COLLATE "pg_catalog"."default",
  "is_applied_by_check" bool NOT NULL DEFAULT false
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
-- Table structure for rsf_pfcbl_categories
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_pfcbl_categories";
CREATE TABLE "p_rsf"."rsf_pfcbl_categories" (
  "pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
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
  "rsf_gpfcbl_family" int4[],
  "rsf_pf_id" int4,
  "deactivated_in_reporting_asof_date" date,
  "deactivated_by_reporting_cohort_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."rsf_pfcbl_ids"."created_by_reporting_cohort_id" IS 'Zero value indicates parse_template created new rsf_pfcbl_ids and related rsf_ids but as-yet didn''t upload any data or create a cohort to claim these IDs.  If something faltered mid-upload, these IDs are effectively null and should be cleaned-up.  When claimed, cohort will also provide creation timestamp and uploader user_id etc';
COMMENT ON COLUMN "p_rsf"."rsf_pfcbl_ids"."created_in_reporting_asof_date" IS 'Convenient field joins to reporting_cohorts''s rsf_reporting reporting_asof_date, set by parse_template''s claim cohort ';

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
-- Table structure for rsf_setup_archive
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_setup_archive";
CREATE TABLE "p_rsf"."rsf_setup_archive" (
  "archive_id" int4 NOT NULL DEFAULT nextval('"p_rsf".rsf_settings_archive_archive_id_seq'::regclass),
  "sys_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "reporting_asof_date" date NOT NULL,
  "pfcbl_name" text COLLATE "pg_catalog"."default" NOT NULL,
  "pfcbl_category" text COLLATE "pg_catalog"."default" NOT NULL,
  "settings_source" text COLLATE "pg_catalog"."default" NOT NULL,
  "settings_value" jsonb,
  "archive_time" timestamptz(6) NOT NULL DEFAULT now(),
  "is_disabled" bool NOT NULL DEFAULT false,
  "is_restored" bool NOT NULL DEFAULT false,
  "restored_by_reporting_cohort_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."rsf_setup_archive"."is_disabled" IS 'To mark for not restoring';

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
  "config_id" int4 NOT NULL DEFAULT nextval('"p_rsf".rsf_setup_checks_config_config_id_seq'::regclass),
  "rsf_pfcbl_id" int4 NOT NULL,
  "for_indicator_id" int4 NOT NULL,
  "indicator_check_id" int4 NOT NULL,
  "check_formula_id" int4,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "config_auto_resolve" bool NOT NULL,
  "config_check_class" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT false,
  "config_threshold" numeric NOT NULL DEFAULT 0.0,
  "config_comments" text COLLATE "pg_catalog"."default",
  "comments_user_id" text COLLATE "pg_catalog"."default",
  "auto_subscribed_by_reporting_cohort_id" int4,
  "config_apply_asof_date" date
)
;
COMMENT ON COLUMN "p_rsf"."rsf_setup_checks_config"."config_apply_asof_date" IS 'only apply on/after this reporting_asof_date';

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
-- Table structure for rsf_setup_template_headers
-- ----------------------------
DROP TABLE IF EXISTS "p_rsf"."rsf_setup_template_headers";
CREATE TABLE "p_rsf"."rsf_setup_template_headers" (
  "rsf_pfcbl_id" int4 NOT NULL,
  "template_id" int4 NOT NULL,
  "rsf_program_id" int4 NOT NULL,
  "rsf_facility_id" int4,
  "header_id" int4 NOT NULL DEFAULT nextval('"p_rsf".rsf_program_facility_template_headers_header_id_seq'::regclass),
  "template_header_sheet_name" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT ''::text,
  "template_header" text COLLATE "pg_catalog"."default" NOT NULL,
  "!dep-template_header_sheet_index" text COLLATE "pg_catalog"."default" DEFAULT 0,
  "action" text COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'default'::text,
  "action_mapping" text COLLATE "pg_catalog"."default" NOT NULL,
  "comment" text COLLATE "pg_catalog"."default",
  "map_indicator_id" int4,
  "map_formula_id" int4,
  "map_check_formula_id" int4,
  "template_header_full_normalized" text COLLATE "pg_catalog"."default" NOT NULL,
  "created_time" timestamptz(6) NOT NULL DEFAULT now(),
  "created_by_user_id" text COLLATE "pg_catalog"."default" NOT NULL,
  "created_by_reporting_cohort_id" int4
)
;
COMMENT ON COLUMN "p_rsf"."rsf_setup_template_headers"."template_header_sheet_name" IS ''''' means all/any sheets encountered';
COMMENT ON COLUMN "p_rsf"."rsf_setup_template_headers"."!dep-template_header_sheet_index" IS '-> migrated into string parsing in view -> NULL means not used; otherwise, is a specific row number integer or column letter for Excel templates to expressly tie a given header to a specific reporting_template_row_id when parsing templtes';
COMMENT ON COLUMN "p_rsf"."rsf_setup_template_headers"."action" IS 'default, ignore, remap';
COMMENT ON COLUMN "p_rsf"."rsf_setup_template_headers"."action_mapping" IS 'to enable unique constraint';

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
  update p_rsf.rsf_setup_template_headers fth  
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
DROP FUNCTION IF EXISTS "p_rsf"."data_value_is_meaningfully_different"("input_rsf_pfcbl_id" int4, "input_indicator_id" int4, "input_reporting_asof_date" date, "input_data_value" text, "input_data_unit" text, "is_user_reporting" bool);
CREATE FUNCTION "p_rsf"."data_value_is_meaningfully_different"("input_rsf_pfcbl_id" int4, "input_indicator_id" int4, "input_reporting_asof_date" date, "input_data_value" text, "input_data_unit" text, "is_user_reporting" bool=false)
  RETURNS "pg_catalog"."bool" AS $BODY$
declare existing_data_id int;
declare existing_data_value text;
declare existing_data_unit text;
declare existing_data_unit_current text;
declare existing_data_reporting_asof_date date;
declare existing_data_cohort_id int;
declare existing_sys_flags int;
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
		rd.reporting_cohort_id,
    rd.data_sys_flags
		into 
		existing_data_value, existing_data_unit, existing_data_id, existing_data_unit_current, existing_data_reporting_asof_date, existing_data_cohort_id, existing_sys_flags
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
	 
   
   /*
select * from p_rsf.rsf_data_sys_flags order by data_flag_value
2	  DELETED
      4	  MANUAL
      8	  CALCULATE
      16	CORRECTION
32	IMMUTABLE
*/

   -- system calculator is checking to insert calculated data for same asof date when MANUAL flag is set to TRUE on the current data point: reject.
   if (is_user_reporting is false) AND (existing_sys_flags&4)=4 AND existing_data_reporting_asof_date is not distinct from input_reporting_asof_date
   then
    return false;
   end if;
   
   
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
				-- if its inactive we don't want to spam it with zeros or nulls.
				-- but inactive loans may still report recoveries.
		    AND (NULLIF(input_data_value,'0') is NOT NULL
				     OR
						 (NULLIF(existing_data_value,'0') is NOT NULL AND NULLIF(input_data_value,'0') is NULL)
				     OR
             (exists(select * from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_id = input_rsf_pfcbl_id and ids.deactivated_in_reporting_asof_date is NULL))
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
     -- because this means that the system value overwrote the user value ... and if the user is uploading the same thing (again) we will overwrite it (again) and generate noise.
		 select
			 rd.data_value,
			 rd.data_unit,  -- IMPORTANT: rd.data_unit and NOT rdc.data_unit because rd.data_unit is what is submitted, and for LCU/LCY, rdc is adjusted!!			 
       rd.reporting_asof_date,
       rd.data_sys_flags
			 into 
			 -- NOTE: existing_data_id is NOT updated here, data_id remains latest-added in time/sequence
			 existing_data_value, 
       existing_data_unit,
       existing_data_reporting_asof_date,
       existing_sys_flags
		 from p_rsf.rsf_data rd 
     inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
		 where rd.rsf_pfcbl_id = input_rsf_pfcbl_id
			 and rd.indicator_id = input_indicator_id
			 and rd.reporting_asof_date <= input_reporting_asof_date
			 and rc.is_reported_cohort is true
			 and (coalesce(rd.data_sys_flags,0) & 2)=0 -- BIT 2 = SOFT DELETE FLAG
		 order by rd.reporting_asof_date desc, rd.data_id desc
		 limit 1;
    
      -- user reporting and last user reported value is set to CALCULATE and currentest datapoint is calculated
      /*
     if (is_user_reporting is true) AND (existing_sys_flags&8)=8 AND existing_data_reporting_asof_date is not distinct from input_reporting_asof_date
     then
      return false;
     end if;
     */
     
    -- if it is different, then insert the change
		if existing_data_value is distinct from input_data_value OR
			 existing_data_unit is distinct from input_data_unit
		then					 
				return true;
    -- the current value is system reported value and the current user value is not different BUT
    -- the current formula is set overwrite to DENY, meaning we should accept user reported data as prevailing value, unless it's explicitly flagged otherwise
    elseif exists(select true
                  from p_rsf.view_rsf_setup_indicator_subscriptions  sis
                  where sis.rsf_pfcbl_id = input_rsf_pfcbl_id
                    and sis.indicator_id = input_indicator_id
                    and sis.formula_overwrite is not distinct from 'deny')
           and (coalesce(existing_sys_flags,0)&8) <> 8 
           and existing_data_reporting_asof_date is not distinct from input_reporting_asof_date
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
    
  	raise notice 'delete_rsf_id(%) %',(select count(*) from deleted_rsf_data),(clock_timestamp()-msg_time);

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
-- Function structure for function_rsf_setup_restore
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."function_rsf_setup_restore"("v_sys_name" text);
CREATE FUNCTION "p_rsf"."function_rsf_setup_restore"("v_sys_name" text)
  RETURNS TABLE("archive_id" int4, "settings_source" text, "sys_name" text) AS $BODY$
begin

return query

with restore as (
  select *  from (
    select distinct on (ids.rsf_pfcbl_id,rsa.settings_source)
      rsa.archive_id,
      ids.rsf_pfcbl_id,
      ids.rsf_program_id,
      ids.rsf_facility_id,
      nids.sys_name,
      rsa.settings_source,
      rsa.settings_value,
      rc.reporting_cohort_id,
      rc.reporting_user_id,
      rc.reporting_type,
      rsa.is_restored,
      rsa.is_disabled
    from p_rsf.rsf_setup_archive rsa
    inner join p_rsf.rsf_data_current_names_and_ids nids on nids.sys_name = rsa.sys_name
                                                        and nids.reporting_asof_date = rsa.reporting_asof_date
                                                        and nids.pfcbl_category = rsa.pfcbl_category
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = nids.rsf_pfcbl_id
    inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = ids.created_by_reporting_cohort_id
    where rsa.sys_name = v_sys_name
      and rsa.is_disabled is false
    order by ids.rsf_pfcbl_id,rsa.settings_source,rsa.archive_time desc
  ) as x
  where x.is_restored is distinct from true
),
restore_indicators as (
  insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
                                         indicator_id,
                                         formula_id,
                                         rsf_program_id,
                                         rsf_facility_id,
                                         is_subscribed,
                                         is_auto_subscribed,
                                         sort_preference,
                                         subscription_comments,
                                         comments_user_id,
                                         options_group_id,
                                         formula_calculation_unit,
                                         auto_subscribed_by_reporting_cohort_id)  
  
  select     
    restore.rsf_pfcbl_id,
    rind.indicator_id,
    rind.formula_id,
    restore.rsf_program_id,
    restore.rsf_facility_id,
    rind.is_subscribed,
    rind.is_auto_subscribed,
    rind.sort_preference,
    rind.subscription_comments,
    rind.comments_user_id,
    rind.options_group_id,
    rind.formula_calculation_unit,
    NULL::int auto_subscribed_by_reporting_cohort_id -- chekc that when auto subscribed is false then no cohort_id and we're only restoring non-auto-subscribed
  from restore  
  inner join lateral jsonb_to_recordset(restore.settings_value) 
                  as rind(indicator_id int,
                          formula_id int,
                          is_subscribed bool,
                          is_auto_subscribed bool,
                          sort_preference int,
                          subscription_comments text,
                          comments_user_id text,
                          options_group_id int,
                          formula_calculation_unit text
                  ) on true
  where restore.settings_source = 'rsf_setup_indicators'
  and exists(select * from p_rsf.indicators ind where ind.indicator_id = rind.indicator_id)  
  and (rind.formula_id is NULL or exists(select * from p_rsf.indicator_formulas indf where indf.formula_id is not distinct from rind.formula_id))
  and rind.is_auto_subscribed is false -- because we will re-auto subscribe where relevant.
  and rind.comments_user_id is NOT NULL
  on conflict do nothing
  returning 'rsf_setup_indicators' as settings_source,rsf_pfcbl_id
),

restore_checks as (
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
  select     
    restore.rsf_pfcbl_id,
    rchk.check_formula_id,
    rchk.indicator_check_id,
    restore.rsf_program_id,
    restore.rsf_facility_id,
    rchk.is_subscribed,
    rchk.is_auto_subscribed,
    rchk.subscription_comments,
    rchk.comments_user_id,
    NULL as auto_subscribed_by_reporting_cohort_id
  from restore  
  inner join lateral jsonb_to_recordset(restore.settings_value) 
                  as rchk(check_formula_id int,
                          indicator_check_id int,
                          is_subscribed bool,
                          is_auto_subscribed bool,
                          subscription_comments text,
                          comments_user_id text
                  ) on true
  where restore.settings_source = 'rsf_setup_checks'
  and exists(select * from p_rsf.indicator_checks ic where ic.indicator_check_id = rchk.indicator_check_id)
  and exists(select * from p_rsf.indicator_check_formulas icf where icf.check_formula_id = rchk.check_formula_id)
  and rchk.is_auto_subscribed is false                                     
  and rchk.comments_user_id is NOT NULL
  on conflict do nothing
  returning 'rsf_setup_checks' as settings_source,rsf_pfcbl_id
),
restore_config as (
  insert into p_rsf.rsf_setup_checks_config(config_id,
                                            rsf_pfcbl_id,
                                            for_indicator_id,
                                            indicator_check_id,
                                            check_formula_id,
                                            rsf_program_id,
                                            rsf_facility_id,
                                            config_auto_resolve,
                                            config_check_class,
                                            config_threshold,
                                            config_apply_asof_date,
                                            config_comments,
                                            comments_user_id,
                                            auto_subscribed_by_reporting_cohort_id)

  select     
    rconfig.config_id,
    restore.rsf_pfcbl_id,
    rconfig.for_indicator_id,
    rconfig.indicator_check_id,
    rconfig.check_formula_id,
    restore.rsf_program_id,
    restore.rsf_facility_id,
    rconfig.config_auto_resolve,
    rconfig.config_check_class,
    rconfig.config_threshold,
    rconfig.config_apply_asof_date,
    rconfig.config_comments,
    rconfig.comments_user_id,
    restore.reporting_cohort_id as auto_subscribed_by_reporting_cohort_id    
  from restore  
  inner join lateral jsonb_to_recordset(restore.settings_value) 
                  as rconfig(
                      config_id int,
                      for_indicator_id int,
                      indicator_check_id int,
                      check_formula_id int,
                      config_auto_resolve bool,
                      config_check_class text,
                      config_threshold numeric,
                      config_apply_asof_date date,
                      config_comments text,
                      comments_user_id text
                  ) on true
  where restore.settings_source = 'rsf_setup_checks_config'
  and not exists(select * from p_rsf.rsf_setup_checks_config scc where scc.config_id = rconfig.config_id)
  and exists(select * from p_rsf.indicators ind where ind.indicator_id = rconfig.for_indicator_id)
  and exists(select * from p_rsf.indicator_checks ic where ic.indicator_check_id = rconfig.indicator_check_id)   
  and (rconfig.check_formula_id is NULL or exists(select * from p_rsf.indicator_check_formulas icf where icf.check_formula_id is not distinct from rconfig.check_formula_id))

  on conflict do nothing
  returning 'rsf_setup_checks_config' as settings_source,rsf_pfcbl_id
),




restore_permissions as (
  insert into users.permissions(account_id,rsf_pfcbl_id,sys_name,granted,denied,notes)
  select 
    rusers.account_id,
    restore.rsf_pfcbl_id,
    restore.sys_name,
    rusers.granted,
    rusers.denied,
    rusers.notes
  from restore  
  inner join lateral jsonb_to_recordset(restore.settings_value) 
                  as rusers(account_id text,
                            granted int,
                            denied int,
                            notes text) on true
  where restore.settings_source = 'users.permissions'
  on conflict do nothing
  returning 'users.permissions' as settings_source,rsf_pfcbl_id
),
restore_headers as (

insert into p_rsf.rsf_setup_template_headers(rsf_pfcbl_id,
                                             template_id,
                                             rsf_program_id,
                                             rsf_facility_id,
                                             header_id,
                                             template_header_sheet_name,
                                             template_header,
                                             action,
                                             action_mapping,
                                             comment,
                                             map_indicator_id,
                                             map_formula_id,
                                             map_check_formula_id,
                                             template_header_full_normalized,
                                             created_by_user_id,
                                             created_by_reporting_cohort_id)
select     
    restore.rsf_pfcbl_id,
    rheaders.template_id,
    restore.rsf_program_id,
    restore.rsf_facility_id,
    rheaders.header_id,    
    rheaders.template_header_sheet_name,    
    rheaders.template_header,
    rheaders.action,
    rheaders.action_mapping,
    rheaders.comment,
    rheaders.map_indicator_id,
    rheaders.map_formula_id,
    rheaders.map_check_formula_id,
    rheaders.template_header_full_normalized,
    restore.reporting_user_id as created_by_user_id,
    restore.reporting_cohort_id as created_by_reporting_cohort_id
  from restore  
  inner join lateral jsonb_to_recordset(restore.settings_value) 
                  as rheaders(template_id int,
                              header_id int,
                              template_header_sheet_name text,
                              template_header text,
                              --template_header_encounter_index int,
                              action text,
                              action_mapping text,
                              comment text,
                              map_indicator_id int,
                              map_formula_id int,
                              map_check_formula_id int,
                              template_header_full_normalized text) on true
  where restore.settings_source = 'rsf_setup_template_headers'
  and exists(select * from p_rsf.reporting_templates rt
             where rt.template_id = rheaders.template_id)
  and (rheaders.map_indicator_id is NULL or exists(select * from p_rsf.indicators ind where ind.indicator_id is not distinct from rheaders.map_indicator_id))
  and (rheaders.map_formula_id is NULL or exists(select * from p_rsf.indicator_formulas indf where indf.formula_id is not distinct from rheaders.map_formula_id))
  and (rheaders.map_check_formula_id is NULL or exists(select * from p_rsf.indicator_check_formulas icf where icf.check_formula_id is not distinct from rheaders.map_check_formula_id))
  and (rheaders.template_header ~ '\*'::text) = false -- else the check will fail
  on conflict do nothing
  returning 'rsf_setup_template_headers' as settings_source,rsf_pfcbl_id
),
restore_archive as (
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
                                            data_sys_flags,
                                            data_value_unit,
                                            data_correction_date)
  
  select     
    rarchive.archive_id,
    now()::timestamptz as archive_time,
    rarchive.sys_name,
    NULL::int as rsf_pfcbl_id,
    rarchive.indicator_id,
    rarchive.indicator_check_id,
    rarchive.check_formula_id,
    rarchive.check_asof_date,
    rarchive.check_status,
    rarchive.status_time,
    rarchive.check_status_user_id,
    rarchive.check_status_comment,
    rarchive.check_message,
    rarchive.data_sys_flags,
    rarchive.data_value_unit,
    rarchive.data_correction_date
    
  from restore  
  inner join lateral jsonb_to_recordset(restore.settings_value) 
                  as rarchive(
                      archive_id int,
                      archive_time timestamptz,
                      sys_name text,
                      indicator_id int,
                      indicator_check_id int,
                      check_formula_id int,
                      check_asof_date date,
                      check_status text,
                      status_time timestamptz,
                      check_status_user_id text,
                      check_status_comment text,
                      check_message text,
                      data_sys_flags int,
                      data_value_unit text,
                      data_correction_date date
                  ) on true
  where restore.settings_source = 'rsf_data_checks_archive'
  and not exists(select * from p_rsf.rsf_data_checks_archive dca where dca.archive_id = rarchive.archive_id)
  and not exists(select * from p_rsf.rsf_data_checks rdc where rdc.evaluation_id = rarchive.archive_id)
  and exists(select * from p_rsf.indicators ind where ind.indicator_id = rarchive.indicator_id)
  and exists(select * from p_rsf.indicator_checks ic where ic.indicator_check_id = rarchive.indicator_check_id)
  and (rarchive.check_formula_id is NULL or exists(select * from p_rsf.indicator_check_formulas icf where icf.check_formula_id is not distinct from rarchive.check_formula_id))  
  on conflict do nothing
  returning 'rsf_data_checks_archive' as settings_source,NULL::int as rsf_pfcbl_id
),
restored as (

  select r.settings_source,r.rsf_pfcbl_id
  from restore_permissions r
  
  union
  
  select r.settings_source,r.rsf_pfcbl_id
  from restore_indicators r
  
  union 

  select r.settings_source,r.rsf_pfcbl_id
  from restore_checks r
  
  union 
  
  select r.settings_source,r.rsf_pfcbl_id
  from restore_config r  
  
  union 
  
  select r.settings_source,r.rsf_pfcbl_id
  from restore_headers r
  
  union 
  
  select r.settings_source,r.rsf_pfcbl_id
  from restore_archive r
)

update p_rsf.rsf_setup_archive rsa
set is_restored = true,
    restored_by_reporting_cohort_id = restore.reporting_cohort_id
from restored
inner join restore on restore.settings_source = restored.settings_source
                  
where rsa.archive_id = restore.archive_id
  and coalesce(restore.rsf_pfcbl_id = restored.rsf_pfcbl_id,true)
returning rsa.archive_id,rsa.settings_source,rsa.sys_name;                  

end $BODY$
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

-- Notes: July2026: This is a key function whose efficiency can be improved and to remove family tree references since currency_ratio indicators 
-- are fixed at facility or global levels only
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
						   exists(select true from p_rsf.rsf_data_current rdc
					            where rdc.indicator_id = ind.indicator_id
											  and rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id)
							 and 
               
               exists(select true from p_rsf.view_rsf_setup_indicator_subscriptions sis
                      where sis.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                        and sis.indicator_id = ind.indicator_id
                        and sis.is_subscribed is true)
						   -- is the requested entity subscribed to the indicator?
						   -- fx indicators only at facility or global levels
							 -- if facility is UN-subscribed, will return false and return global-level
							 -- if facility is empty, will return subscription at program level (true or false)
							 -- if program is empty, will return NULL is true (false)
              
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
    
		end if;
		
		is_invalidated := exists(select true 
		                         from p_rsf.rsf_data_calculation_evaluations rdce
														 where rdce.rsf_pfcbl_id = v_fx_rsf_pfcbl_id
														   and rdce.indicator_id = fx_indicator_id
															 and rdce.calculation_asof_date = exchange_rate_date);
                               
		is_unreported := exchange_rate_data_id is NULL 
		                 or
										 not exists(select true
		                            from p_rsf.rsf_data_calculation_validations dcv
													      where dcv.data_id = exchange_rate_data_id
														      and dcv.calculation_asof_date = exchange_rate_date);
                               
/*															 
		is_unreported := exchange_rate_data_id is NULL 
		                 or
										 not exists(select true
		                            from p_rsf.rsf_pfcbl_reporting rpr
													      where rpr.rsf_pfcbl_id = v_fx_rsf_pfcbl_id
														      and rpr.reporting_asof_date = exchange_rate_date);
*/		                        
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


    insert into p_rsf.reporting_dates(quarter_end_date)
    select 
    grd.quarter_end_date
    from p_rsf.rsf_pfcbl_generate_reporting_dates(0,now()::date) grd 
    on conflict do nothing;    
    
    FOR reporting_dates IN
			select 
      grd.quarter_end_date
      from p_rsf.reporting_dates qrd
      where not exists(select true
                       from p_rsf.reporting_imports ri
                       where rpr.rsf_pfcbl_id = 0
                         and rpr.reporting_asof_date = grd.quarter_end_date)
      order by 
      grd.quarter_end_date                    
    LOOP
    
       raise notice 'global_reporting creating new reporting_cohort entry for: %',reporting_dates.quarter_end_date;

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
         reporting_dates.quarter_end_date as reporting_asof_date,
         ri.template_id,
         ri.file_name,
         ''::bytea,
         concat('Global Reporting Triggered by: ',ri.file_name,' import_id=',ri.import_id) as import_comments,
         'GLOBAL'
         from p_rsf.reporting_imports ri
         where ri.import_id = NEW.import_id
           and not exists(select true 
                          from p_rsf.reporting_imports ri
                          where ri.import_rsf_pfcbl_id = 0
                            and ri.reporting_asof_date = reporting_dates.quarter_end_date)
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
	
  if NEW.data_type = 'currency' AND NEW.data_unit <> 'LCU' and NEW.unit_fx_indicator_id is NULL
  then 
    raise exception 'Defined currency indicators (here in %) must set a unit_fx_indicator_id that governs FX relative to a paired LCU indicator.  unit_fx_indicator_id cannot be blank.',
    NEW.data_unit;
    return NULL;
  end if;
  
  if NEW.unit_fx_indicator_id is NOT NULL
     AND
     not exists(select * from p_rsf.indicators ind
                where ind.indicator_id = NEW.unit_fx_indicator_id
                  and ind.data_type = 'currency'
                  and ind.data_unit = 'LCU'
                  and ind.data_category = NEW.data_category)
  then
    raise exception 'Defined currency indicators (here in %) must set a unit_fx_indicator_id paried to an indicator that is also %-level and an LCU currency data type',
    NEW.data_unit,NEW.data_category;
    return NULL;
  end if;
           
  if NEW.unit_fx_indicator_id is NOT NULL
     AND 
     exists(select * from p_rsf.indicator_formulas indf
            where indf.indicator_id = NEW.indicator_id)
  then 
    raise exception 'Defined currency indicators (here in %) cannot simultaneously define formulas (the formula is calculated by the indicator it is paired to)',
    NEW.data_unit;
    return NULL;
  
  end if;
           
  if NEW.indicator_name ~* 'list' AND NEW.data_type = 'text' then
    NEW.indicator_options_group_allows_multiples := true;
  end if;
  
  if NEW.indicator_options_group_allows_multiples is true 
     AND NOT (NEW.indicator_options_group_id is NOT NULL
              OR
              (NEW.data_type = 'text' AND NEW.indicator_name ~* 'list')
             ) then
              
     NEW.indicator_options_group_allows_multiples := NULL;
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
        and ind.data_category = 'global'
      on conflict do nothing;
    end if;    
  

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

  union all   
  
  select 
	  iids.rsf_pfcbl_id,
		ind.indicator_id,
		iids.created_in_reporting_asof_date,
		iids.created_by_reporting_cohort_id,
		NULL::text as data_value,
		ind.data_unit,
    '{CREATED SYSID ' || iids.rsf_pfcbl_id::text || '}' as data_submitted
	from inserted_ids iids 
	inner join p_rsf.indicators ind on ind.data_category = iids.pfcbl_category
	where ind.is_required is true --ind.indicator_sys_category in ('id','rank_id','is_active')

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
	
  --Note: This trigger should ensure that all calculations have an initial calculation entry.
  --And therfore p_rsf.view_rsf_pf_calculation_evaluations_required does not always need to check this, strictly.
	insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
	                                                   indicator_id,
																										 calculation_asof_date,
                                                     rsf_pf_id,
                                                     formula_calculation_rank)
	select
		iids.rsf_pfcbl_id,
		rsi.indicator_id,
		iids.created_in_reporting_asof_date as calculation_asof_date,
    iids.rsf_pf_id,
    coalesce(indf.formula_calculation_rank,0) as formula_calculation_rank
	from inserted_ids iids 
	inner join p_rsf.rsf_setup_indicators rsi on rsi.rsf_pfcbl_id = iids.rsf_pf_id
  inner join p_rsf.indicators ind on ind.indicator_id = rsi.indicator_id    
                                 and ind.pfcbl_rank = iids.pfcbl_category_rank                             
  left join p_rsf.indicator_formulas indf on indf.formula_id = rsi.formula_id
	where rsi.is_subscribed = true	
    
    and (rsi.formula_id is not null or ind.unit_fx_indicator_id is not null)    
	on conflict
	do nothing;

	raise info 'insert_rsf_pfcbl_id_evaluations initialized rsf_data_calculation_evaluations in %',
	(select clock_timestamp()-msg_time);
	
	
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
	
	-- having initialized the family, now initalize the new entity's LCU unit, which is overwhelmingly here at initialization
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
    inner join p_rsf.indicators ind on ind.pfcbl_rank = ft.to_pfcbl_rank
                                   and ind.indicator_sys_category = 'entity_local_currency_unit' -- not defined, therefor is_defined_lcu is false
    inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                         and rdc.indicator_id = ind.indicator_id
    where rdc.reporting_asof_date <= iids.created_in_reporting_asof_date
      and ft.pfcbl_hierarchy <> 'child'
    order by
      iids.rsf_pfcbl_id,
      ft.to_pfcbl_rank desc,  -- ie, first facility=2, then program=1
      rdc.reporting_asof_date desc; -- in case the LCU has changed over time, eg, country devalues or reissues (ie, Ghana Cedi vs Shilling)
                                         

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
	
	raise notice 'reporting_cohort_group_deleted(%) TG_OP=% and trigger_depth=%',
  (select array_agg(reporting_cohort_id) from deleted_reporting_cohorts),TG_OP,pg_trigger_depth();					 

  with archive_checks as (
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
                                            data_sys_flags,
                                            data_value_unit,
                                            data_correction_date
                                            --evaluation_data
                                            )
    select 
    cae.evaluation_id as archive_id,
    now() as archive_time,
    cae.archive_sys_name,
    cae.rsf_pfcbl_id,
    cae.indicator_id,
    cae.indicator_check_id,
    cae.check_formula_id,
    cae.check_asof_date,
    cae.check_status,
    cae.status_time,
    cae.check_status_user_id,
    cae.check_status_comment,
    cae.check_message,
    cae.data_sys_flags, -- only save the data flags that were put on the flag, not the data itself.
    cae.data_value_unit,
    cae.data_correction_date
    --cae.evaluation_data
    from p_rsf.rsf_data_checks as cae
    where cae.archive_sys_name is not null
      and (cae.data_id = any(select rd.data_id      
                             from p_rsf.rsf_data rd 
                             where rd.reporting_cohort_id = any(select drc.reporting_cohort_id from deleted_reporting_cohorts drc))
           or 
           cae.for_import_id = any(select drc.import_id from deleted_reporting_cohorts drc))
  
    returning archive_id
   )
   delete from p_rsf.rsf_data_checks rdc
   using archive_checks ac
   where ac.archive_id = rdc.evaluation_id;

	raise notice 'reporting_cohort_group_deleted: checks archived and deleted in %',(clock_timestamp()-msg_time);
	msg_time:= clock_timestamp();


/* disabled for testing
  if exists(select * from deleted_reporting_cohorts drc
            where not exists(select * from p_rsf.reporting_imports_deleted_archive ida
                             where ida.import_id = drc.import_id))
  then
    raise exception 'Failed to delete reporting cohort because import_id is not present in reporting_imports_deleted_archive.  To trigger deletion use: insert into p_rsf.reporting_imports_deleted_archive(import_id,deleting_user_id) values(import_id,user_id)';
  end if;
*/  

	
	delete from p_rsf.rsf_data_calculation_evaluations dce
	where dce.rsf_pfcbl_id = any(select ids.rsf_pfcbl_id 
                               from p_rsf.rsf_pfcbl_ids ids
                               where ids.created_by_reporting_cohort_id =  any(select drc.reporting_cohort_id from deleted_reporting_cohorts drc));

	raise notice 'reporting_cohort_group_deleted: pending for deleted entities removed from rsf_data_calculation_evaluations: %',(clock_timestamp()-msg_time);
	msg_time:= clock_timestamp();

	delete from p_rsf.rsf_data_calculation_validations dcv
	where dcv.rsf_pfcbl_id = any(select ids.rsf_pfcbl_id 
                               from p_rsf.rsf_pfcbl_ids ids
                               where ids.created_by_reporting_cohort_id =  any(select drc.reporting_cohort_id from deleted_reporting_cohorts drc));
                   
  raise notice 'reporting_cohort_group_deleted: pending for deleted entities removed from rsf_data_calculation_validations: %',(clock_timestamp()-msg_time);
	msg_time:= clock_timestamp();

	-- delete rsf_pfcbl_ids FIRST to ensure backup archives related to rsf_setup_archive have full data, sys_names, etc available to backup and no fk triggers have been called.
	delete from p_rsf.rsf_pfcbl_ids ids
	where ids.created_by_reporting_cohort_id = any(select drc.reporting_cohort_id from deleted_reporting_cohorts drc);
	
	raise notice 'reporting_cohort_group_deleted: rsf_pfcbl_ids created by this cohort %',(clock_timestamp()-msg_time);
	msg_time:= clock_timestamp();
	
	delete from p_rsf.rsf_data rd
	where rd.reporting_cohort_id = any(select drc.reporting_cohort_id from deleted_reporting_cohorts drc);

	raise notice 'reporting_cohort_group_deleted rsf_data! %',(clock_timestamp()-msg_time);
	msg_time:= clock_timestamp();


  -- reporting_imports insert into reporting_imports_archive should trigger the deletion of cohorts.  But to ensure this constraint, it's applied here on the entire table.
  -- manual processes or system maintinenance can cause a lapse.
  delete from p_rsf.reporting_imports ri
  where not exists(select * from p_rsf.rsf_pfcbl_ids ids 
                   where ids.rsf_pfcbl_id = ri.import_rsf_pfcbl_id);
                   
	raise notice 'reporting_cohort_group_deleted: done! %',(clock_timestamp()-msg_time);
	msg_time:= clock_timestamp();
                   
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
-- Function structure for reporting_imports_set_sequence_name
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."reporting_imports_set_sequence_name"();
CREATE FUNCTION "p_rsf"."reporting_imports_set_sequence_name"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE rank_prefix text;
BEGIN  

  if (not exists(select * from p_rsf.rsf_pfcbl_ids ids
                 where ids.rsf_pfcbl_id = NEW.import_rsf_pfcbl_id
                   and ids.pfcbl_category_rank <= 2))
  then                    
  
    raise exception 'Reporting imports may only be at the Global, Program or Facility levels';
  end if;                   
  
  
  select 
  lpad(dates.reporting_sequence_rank::text,greatest(char_length(dates.reporting_sequence_rank::text),2),'0') as reporting_rank  
  into rank_prefix
  from p_rsf.rsf_pfcbl_generate_reporting_dates(v_rsf_pfcbl_id => NEW.import_rsf_pfcbl_id,
                                                v_until_date => NEW.reporting_asof_date) as dates 
  where dates.valid_reporting_date = NEW.reporting_asof_date;

  if exists(select * from p_rsf.rsf_pfcbl_ids ids 
            where ids.rsf_pfcbl_id = NEW.import_rsf_pfcbl_id
              and ids.created_in_reporting_asof_date = NEW.reporting_asof_date)
     AND 
     
     exists(select * from p_rsf.reporting_templates rt 
            where rt.template_id = NEW.template_id
              and (rt.is_system is true OR rt.template_name = 'IFC-RSA-TEMPLATE'))
  then
    rank_prefix := '00';
  end if;
  
  rank_prefix := coalesce(rank_prefix,'00');
  
  NEW.file_name := trim(NEW.file_name);
  
  -- limit to ,3} so if they've started with a project ID number we don't accidentally overwrite it.
  if (NEW.file_name ~ '^#?[0-9]{1,3}\s*[_-]?\s*|^#\s+') -- provides its own rank prefix (correct or not?)
  then 
    NEW.file_name := regexp_replace(NEW.file_name,'^#?[0-9]{1,3}\s*[_-]\s*?|^#\s+',concat('#',rank_prefix,' ')); 
    
  elseif (NEW.file_name ~* '^[a-z]') -- provides no rank prefix
  then 
    NEW.file_name := concat('#',rank_prefix,' ',NEW.file_name);
  
  elseif (NEW.file_name ~* '^[0-9]{4,}') -- starts with a client or project ID
  then 
    NEW.file_name := concat('#',rank_prefix,' ',NEW.file_name);
  
  
  end if;
   
  NEW.file_name := regexp_replace(NEW.file_name,'\s{2,}',' ');
  
return NEW;


END $BODY$
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
-- Function structure for rsf_data_calculation_evaluation_error_check
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_calculation_evaluation_error_check"();
CREATE FUNCTION "p_rsf"."rsf_data_calculation_evaluation_error_check"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN 

if not exists(select true from chk) then return NULL; end if;

if (exists(select true from p_rsf.error_check_calculation_evaluations))
then
    raise exception 'Calculation evaluation error triggered';
end if;

raise notice ' * Debugging - Error check enabled for rsf_data_calculation_evaluation_error_check() trigger';

return NULL;

 -- NOTE:
 -- this is retained because calculation entries can/do enter that pre-date the rsf_pfcbl_id creation date when function to recalculate everything is performed.
 if (not exists(select * from p_rsf.rsf_pfcbl_ids ids
                where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                  and ids.created_in_reporting_asof_date <= NEW.calculation_asof_date
                  and ids.pfcbl_category = (select ind.data_category from p_rsf.indicators ind where ind.indicator_id = new.indicator_id)))
 then
    return NULL;
 end if;
     


return NEW; -- IE, do nothing, this trigger is diabled not in testing (but don't return NULL!!!)

 -- this function was originally created for testing purposes.
  --  but is retained to ensure that calculation dates do not pre-date the entity they're asked to calculate following
  --  changes in allowing calculations to be calculated outside an entity's reporting timelines (which allows fx rate fluctuations to trigger recalculations for reporting needs
  --  even after facilities have closed.  But timeseries updates can trigger parent parameters that pre-date entity creation to enter bad timelines.  This is now denied here.
    
 -- Checking pfcbl category is also retained, which also originated in testing and stems from re-classifying metrics; which isn't a valid use scenario but can happen on an exceptional basis.
 

 if (select ids.pfcbl_category from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id) 
    is distinct from
    (select ind.data_category from p_rsf.indicators ind where ind.indicator_id = new.indicator_id)
 then
  raise exception 'Calculation category mistmatch for % to calculate %',
  (select sn.sys_name from p_rsf.view_rsf_pfcbl_id_current_sys_names sn where sn.rsf_pfcbl_id = NEW.rsf_pfcbl_id),
  (select ind.indicator_name from p_rsf.indicators ind where ind.indicator_id = NEW.indicator_id);
  return NULL;  
 end if;
 
 if exists(select true from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_ids = NEW.rsf_pfcbl_id and NEW.rsf_pf_id is distinct from ids.rsf_pf_id)
 then
 
 raise exception 'Calculation rsf_pf_id mismatch for %',NEW.rsf_pf_id;
 
 end if; 
 
 --select * from p_rsf.rsf_data_calculation_evaluations;
 --return NEW;
 
 if (not exists(select * from p_rsf.view_rsf_setup_indicator_subscriptions sis
                where sis.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                  and sis.indicator_id = NEW.indicator_id
                  and sis.is_subscribed is true))
 then
  raise notice 'TESTING in trigger_rsf_data_4_modified-calculations: Attempt to calculate unsubscribed indicator for: rsf_pfcbl_id=% indicator_id=% asof=%',
  NEW.rsf_pfcbl_id,NEW.indicator_id,NEW.calculation_asof_date;
  return NULL;
 end if;
 
 return NEW;
 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_calculation_evaluation_revalidate
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_calculation_evaluation_revalidate"();
CREATE FUNCTION "p_rsf"."rsf_data_calculation_evaluation_revalidate"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN 
 
 with revalidates as (
   delete from p_rsf.rsf_data_calculation_validations dca
   where dca.rsf_pfcbl_id = NEW.rsf_pfcbl_id
     and dca.indicator_id = NEW.indicator_id
     and dca.calculation_asof_date >= NEW.calculation_asof_date -- >= is important as also means that any new evaluation triggers is no longer validated at all regardless of time.
   returning rsf_pfcbl_id,indicator_id,calculation_asof_date
 )
 insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
                                                    indicator_id,
                                                    calculation_asof_date,
                                                    rsf_pf_id,
                                                    formula_calculation_rank)
 select 
  dcv.rsf_pfcbl_id,
  dcv.indicator_id,
  dcv.calculation_asof_date,
  NEW.rsf_pf_id,
  NEW.formula_calculation_rank
 from revalidates dcv
 on conflict do nothing; -- most likely the evaluation that triggers this trigger exists if it had ever been calculated/validated previously; do nothing. 
                                                                   
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
                                                     data_id,
                                                     validation_time)
 select
 OLD.rsf_pfcbl_id,
 OLD.indicator_id,
 OLD.calculation_asof_date,
 (select rdc.data_id
  from p_rsf.rsf_data_current rdc
  where rdc.rsf_pfcbl_id = OLD.rsf_pfcbl_id
    and rdc.indicator_id = OLD.indicator_id
    and rdc.reporting_asof_date <= OLD.calculation_asof_date
  order by rdc.reporting_asof_date desc 
  limit 1),
  (timeofday())::timestamp with time zone as validation_time
 on conflict do nothing;
                                                                   
 return OLD;
 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_check_evaluation_allowed
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_check_evaluation_allowed"();
CREATE FUNCTION "p_rsf"."rsf_data_check_evaluation_allowed"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN 

 
 if (select ids.pfcbl_category from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id) 
    is distinct from
    (select icf.check_pfcbl_category from p_rsf.indicator_check_formulas icf where icf.check_formula_id = NEW.check_formula_id)
 then
  raise exception 'Calculation category mistmatch for % to calculate %',
  (select sn.sys_name from p_rsf.view_rsf_pfcbl_id_current_sys_names sn where sn.rsf_pfcbl_id = NEW.rsf_pfcbl_id),
  (select icf.check_formula_title from p_rsf.indicator_check_formulas icf where icf.check_formula_id = NEW.check_formula_id);
  return NULL;  
 end if;
 
 --select * from p_rsf.rsf_data_calculation_evaluations;
 --return NEW;
 
 if (not exists(select * from p_rsf.view_rsf_setup_check_subscriptions scs
                where scs.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                  and scs.check_formula_id = NEW.check_formula_id
                  and scs.is_subscribed is true))
 then
  raise notice 'TESTING in trigger_rsf_data_5_modified-checks: Attempt to calculate unsubscribed check for: rsf_pfcbl_id=% check_formula_id=% asof=%',
  NEW.rsf_pfcbl_id,NEW.check_formula_id,NEW.check_asof_date;
  return NULL;
 end if;
 
 --raise notice 'rsf_data_check_evaluation_allowed: rsf_pfcbl_id=% check_formula_id=% asof=%',
 -- NEW.rsf_pfcbl_id,NEW.check_formula_id,NEW.check_asof_date;
 /*
 if (NEW.calculation_asof_date is distinct from '2023-09-30'::date)
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
-- Function structure for rsf_data_check_is_current
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_check_is_current"("v_data_id" int4, "v_rsf_pfcbl_id" int4, "v_indicator_id" int4, "v_check_asof_date" date);
CREATE FUNCTION "p_rsf"."rsf_data_check_is_current"("v_data_id" int4, "v_rsf_pfcbl_id" int4, "v_indicator_id" int4, "v_check_asof_date" date)
  RETURNS "pg_catalog"."bool" AS $BODY$
begin

  -- the v_data_id associated with the check (and the v_indicator_id) is the most recently entered current data point for this entity
  if v_data_id is not distinct from (select rdc.data_id 
                                     from p_rsf.rsf_data_current rdc
                                     where rdc.rsf_pfcbl_id = v_rsf_pfcbl_id
                                       and rdc.indicator_id = v_indicator_id
                                       and rdc.reporting_asof_date <= v_check_asof_date                         
                                     order by rdc.reporting_asof_date desc
                                     limit 1)
  then
  
    return true;

  -- if the v_data_id is tagged to entity_reporting, these are current as long as the entity has a reporting entry.    
  elseif exists(select true 
                from p_rsf.rsf_data rd
                inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
                where rd.data_id = v_data_id
                  and ind.indicator_sys_category = 'entity_reporting'
                  and exists(select true
                             from p_rsf.rsf_pfcbl_ids ids
                             where ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
                               and (ids.deactivated_in_reporting_asof_date is NULL
                                    or
                                    ids.deactivated_in_reporting_asof_date >= v_check_asof_date)))
  then 
  
    return true;
  
  else
  
    return false;
    
  end if;
  
return true;
end; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_checks_0_restore_archive
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_checks_0_restore_archive"();
CREATE FUNCTION "p_rsf"."rsf_data_checks_0_restore_archive"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
  DECLARE restore record;
  DECLARE passive_id int default NULL;
BEGIN
  
  --raise notice 'rsf_data_checks_0_restore_archive % % %',NEW.data_id,NEW.rsf_pfcbl_id,NEW.indicator_id;
  
  new.archive_sys_name := (select nai.sys_name
                           from p_rsf.rsf_data_current_names_and_ids nai
                           where nai.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                             and nai.reporting_asof_date <= NEW.check_asof_date
                           order by nai.reporting_asof_date desc
                           limit 1);

  -- NOTE: operates on rsf_data_CURRENT where data_unit may have been adjusted    
  -- NOTE: data_value_unit is used for FLAGGED data restoration since the flag is on the datapoint.  Whereas regular checks are restored against the check message.
  --       consequently, THIS trigger to restore flags compares messages.  See trigger function rsf_data_current_unarchive_checks() for restored data flags.
  --       So the data_value_unit is being set here independently of whether any flags are being restored.     
  new.data_value_unit := (select case when ind.indicator_sys_category = 'entity_reporting' then 'entity_reporting'
                                 else p_rsf.rsf_data_value_unit(rdc.data_value,rdc.data_unit) end
                          from p_rsf.rsf_data_current rdc
                          inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
                          where rdc.data_id = new.data_id);                         
  
  if (NEW.check_formula_id is NULL -- system indicator
      and 
      NEW.data_value_unit is not distinct from 'entity_reporting' -- assigned to reporting indicator
      and 
      NEW.for_import_id is NULL) -- not assigned to an import
  then
    
    NEW.for_import_id := (select 
                          ri.import_id
                          from p_rsf.rsf_pfcbl_ids ids
                          inner join p_rsf.reporting_imports ri on ri.import_rsf_pfcbl_id in (ids.rsf_client_id,ids.rsf_facility_id,ids.rsf_program_id)                          
                          where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                            and ri.reporting_asof_date <= NEW.check_asof_date
                          order by
                          ri.import_rsf_pfcbl_id is not distinct from ids.rsf_client_id desc,
                          ri.import_rsf_pfcbl_id is not distinct from ids.rsf_facility_id desc,
                          ri.import_rsf_pfcbl_id is not distinct from ids.rsf_program_id desc,
                          ri.reporting_asof_date desc
                          limit 1);
                            
  end if;
                  
  select 
    dca.archive_id,
    dca.check_status,
    dca.check_status_comment,
    dca.check_status_user_id,   
    coalesce(new.data_sys_flags,0) | coalesce(dca.data_sys_flags) as data_sys_flags
  into 
    restore
  from p_rsf.rsf_data_checks_archive dca

  where dca.sys_name = new.archive_sys_name
    and dca.indicator_id = new.indicator_id    
    and dca.check_asof_date = new.check_asof_date
    and dca.indicator_check_id = new.indicator_check_id    
    
    and dca.check_formula_id is not distinct from new.check_formula_id
    and dca.check_message is not distinct from new.check_message
    
   order by dca.archive_time desc
   limit 1;
   
       
   
   if (restore.archive_id is not null) 
   then
        --raise info 'Restoring check from % into %',new.evaluation_id,restore.archive_id;
    
    new.evaluation_id := restore.archive_id; -- restore the old ID so that flag exports can re-match deleted flags if they're re-imported.
    new.check_status := restore.check_status;
    new.check_status_comment := restore.check_status_comment;
    new.check_status_user_id := restore.check_status_user_id;
    --new.check_data_id_is_current := true;
    new.data_sys_flags := restore.data_sys_flags;    
    
    delete from p_rsf.rsf_data_checks_archive dca
    where dca.archive_id = restore.archive_id;
    
   end if;

   
   -- is this a contract_criteria check?
   -- inclusion criteria should only be assessed in the period in which the loan is included.
   if (exists(select true 
              from p_rsf.indicator_checks ic 
              where ic.indicator_check_id = new.indicator_check_id 
                and ic.check_type = 'contract_criteria'))
      AND 
      
      -- so we only join the family tree if required since passive breaches are uncommon, especially on parent entities
      NOT exists(select * from p_rsf.rsf_pfcbl_ids ids
                where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id 
                  and ids.created_in_reporting_asof_date = NEW.check_asof_date)
   then
     passive_id := (select ic.indicator_check_id from p_rsf.indicator_checks ic where ic.check_name = 'sys_passive_inclusion_criteria_breach');
     
     if (passive_id is NULL) then
       raise exception 'Failed to find required system check: sys_criteria_passive_breach';
     end if;
     
     if (not exists(select true 
                    from p_rsf.view_rsf_pfcbl_id_family_tree ft
                    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                    where ft.from_rsf_pfcbl_id = NEW.rsf_pfcbl_id
                      and ids.created_in_reporting_asof_date = NEW.check_asof_date
                      and ft.pfcbl_hierarchy <> 'parent'
                      and ft.to_pfcbl_category = 'loan'))
     then
       
       select 
         concat(ic.check_name,
                case when icf.check_formula_title is not null then concat('[',icf.check_formula_title,']')
                     else '' end,
                ' is passive because criteria applies only at inclusion date: ',new.check_message)
       into 
        new.check_message
       from p_rsf.indicator_checks ic
       left join p_rsf.indicator_check_formulas icf on icf.check_formula_id = NEW.check_formula_id
       where ic.indicator_check_id = NEW.indicator_check_id;
       
       new.check_formula_id := NULL;
       new.indicator_check_id := passive_id;
       
     end if;                      
   end if;   
   
   -- only "is_data_check" defined indicator checks can submit data_check_value/unit
   -- and it only matters if it's actually different from the data point that this flag is flagging.
   if exists(select true from p_rsf.indicator_checks ic
           where ic.indicator_check_id = NEW.indicator_check_id
             and ic.is_data_check is true)
                                                                          
   then
     NEW.check_has_data := new.data_value_unit is distinct from p_rsf.rsf_data_value_unit(NEW.data_check_value,NEW.data_check_unit);
   else
      NEW.check_has_data := false;
      NEW.data_check_value := NULL;
      NEW.data_check_unit := NULL;
   end if;
    
    
 -- when a check is being inserted it must have a data_id associated with it.
 -- either the associated data_id (the indicator it's flagging)
 -- or a placeholder data_it pointing to the last-known entity_reporting entry
 
 NEW.check_data_id_is_current := p_rsf.rsf_data_check_is_current(v_data_id => NEW.data_id,
                                                                 v_rsf_pfcbl_id => NEW.rsf_pfcbl_id,
                                                                 v_indicator_id => NEW.indicator_id,
                                                                 v_check_asof_date => NEW.check_asof_date);
                                                                 
                                                                 --select * from p_rsf.indicator_checks where (data_sys_flags_granted&4)=4                                              
 -- MANUAL                                                                 
 -- its a calculated overwrite flag 
 -- and its applied on reported data
 -- This can only happen in very bizarre scenarios where user uploads a correct data; then next quarter wrong data; then calculator re-corrects and overwrites to the historically 
 -- correct data and then these data points are removed as non-changes from rsf_data_current result in the now current data point being the origianl reported data
 -- that is now receiving a calculator overwrite flag (which is confusing, especially as the flag will carry the message using the old reported incorrect data that it did overwrite and
 -- cause to get cleaned-up.
 if exists(select true from p_rsf.indicator_checks ic
            where ic.indicator_check_id = NEW.indicator_check_id
              and ic.is_calculator_check is true                                                  
              and (coalesce(ic.data_sys_flags_granted,0)&4)=4) -- MANUAL tag, meaning this is a system overwrite flag since only calculator overwrite flags can be tagged to force manual.
     
     
 then
 
    if exists(select true from p_rsf.rsf_data rd
              inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
              where rd.data_id = NEW.data_id
                and rc.is_reported_cohort is true)
    then               
     -- Should we just RETURN NULL here to deny the insert of a meaningless and confusing flag?
     -- NO! Because there's a small change that the manually reported data is correct and needs to be retained and this flag needs to be tagged as "manual"
     NEW.check_message := concat(NEW.check_message,' [SYSTEM] This calculator flag has been applied to an historically reported value and is not related to any calculation. ',
                                 'This is caused by unexpected (and incorrect) data having been reported and corrected over time. This flag should be resolved (and ignored) unless ',
                                 'the very rare circumstance that requires this calculated result to be tagged as MANUL and rejected to re-insert the reported value for ',NEW.check_asof_date);
    
    -- We're inserting a new flag with MANUAL granted (ie, user can tag to reject system overwrite to use reported data instead)
    -- and the flag is being applied to a calculated data point
    -- and the last reported data point in the flagged data point has been tagged with 8 (ie, use system calcualtor isntead)
    -- then it means user asked to overwrite reported data; system calculator ran and inserted a new data point and then flagged it (now) as being overwritten.
    -- since this is all expected, the new flag is redundant.  But we allow it to come so that we have the opportunity to re-reject it and rever back to the original data point.
    elseif NEW.check_status_comment is NULL and 
           NEW.check_status = 'active' and
           NEW.check_message ~* 'tagged to accept this system correction' -- check message written because last data point has been tagged as 8 and this is cheaper than re-looking up last data point to verify tag 8.
           
     then                      
      NEW.check_status_comment := concat(NEW.check_status_comment,' [SYSTEM] Auto-resolved because previously reported data was tagged to accept system calculation and overwrite is expected.');  
      NEW.check_status = 'resolved';
      NEW.check_status_user_id := coalesce(NEW.check_status_user_id,
                                      (select account_id from p_rsf.view_account_info where is_system_account is true and users_name = 'RSF SYS Calculator'));                      

    
    
    end if;                    
 end if;
                  
                          
   return new;
   
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_checks_clean_archive
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_checks_clean_archive"();
CREATE FUNCTION "p_rsf"."rsf_data_checks_clean_archive"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN 

  delete from p_rsf.rsf_data_checks_archive dca
  where (dca.archive_time < (now() - interval '90 days'));
   
  return NULL;
 
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_checks_flagged_data_cascade
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_checks_flagged_data_cascade"();
CREATE FUNCTION "p_rsf"."rsf_data_checks_flagged_data_cascade"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE tmp record;
BEGIN

--raise warning 'rsf_data_checks_flagged_data evaluation_id=% flags=%',NEW.evaluation_id,NEW.data_sys_flags;
/*
select * from p_rsf.rsf_data_sys_flags order by data_flag_value
2	  DELETED
4	  MANUAL
8	  CALCULATE
16	CORRECTION
32	IMMUTABLE
*/

if exists(select * from p_rsf.rsf_data rd
          where rd.data_id = NEW.data_id
            and rd.indicator_id is distinct from NEW.indicator_id)
then

  raise exception 'This check cannot be tagged as % because the data point flagged is different from the indicator associated with the check (this can happen when non-reported data is being flagged). There is no underlying data-point for this check on indicator %',
  (select data_flag_name from p_rsf.rsf_data_sys_flags where data_flag_value = NEW.data_sys_flags),
  (select indicator_name from p_rsf.indicators where indicator_id = NEW.indicator_id);
  
end if;

if (TG_OP = 'UPDATE' AND NEW.data_sys_flags is not distinct from OLD.data_sys_flags)
then
  return NULL;
end if;

if (TG_OP = 'INSERT' and coalesce(NEW.data_sys_flags,0) = 0)
then
  return NULL;
end if;

--CORRECTION (of historically reported data)
if (NEW.data_sys_flags & 16)=16
then

  if exists(select * from p_rsf.rsf_data rd
            inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
            where rd.data_id = NEW.data_id
              and rc.is_reported_cohort is not true)
  then
  
    raise exception 'This check cannot be tagged as a CORRECTION: only reported data can be treated as corrections for other historically-reported data (ie, not system calculated data)';    
  
  end if;
  
  select
    coalesce(previous.reporting_asof_date,ids.created_in_reporting_asof_date) as correction_asof_date,
    previous.data_id as incorrect_data_id,
    previous.previous_data_value_unit
  into tmp
  from p_rsf.rsf_pfcbl_ids ids
  left join lateral (select 
                      rdc.data_id,
                      rdc.reporting_asof_date,
                      p_rsf.rsf_data_value_unit(rdc.data_value,rdc.data_unit) as previous_data_value_unit
                     from p_rsf.rsf_data_current rdc
                     inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
                     inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
                     where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                       and rdc.indicator_id = NEW.indicator_id
                       and rdc.reporting_asof_date < NEW.check_asof_date -- < not <= because its an historic correction
                       and rc.is_reported_cohort is true
                     order by
                     rdc.reporting_asof_date desc
                     limit 1) as previous on true  
  where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id;
  
  --raise notice 'New ID % Tmp ID %',new.data_id,tmp.incorrect_data_id;
  if (tmp.incorrect_data_id is not distinct from NEW.data_id)
  then
    raise exception 'Cannot correct a data point to itself. Failed for data_id=%',NEW.data_id;
  end if;


  update p_rsf.rsf_data_checks rdc
  set check_status_comment = concat(NEW.check_status_comment,' [SYSTEM#',tmp.incorrect_data_id,'] ',
                                     NEW.data_value_unit,' marked as a correction in ',NEW.check_asof_date,' to REPLACE ',
                                     tmp.previous_data_value_unit,' reported in ',tmp.correction_asof_date),
      check_status = 'resolved',
      check_status_user_id = coalesce(NEW.check_status_user_id,
                                      (select account_id from p_rsf.view_account_info where is_system_account is true and users_name = 'RSF SYS Calculator')),
      data_correction_data_id = tmp.incorrect_data_id,
      data_correction_date = tmp.correction_asof_date

  where rdc.evaluation_id = NEW.evaluation_id;
  

  -- will not trigger anything.
  delete from p_rsf.rsf_data_current rdc
  where rdc.data_id = tmp.incorrect_data_id
     or rdc.data_id = NEW.data_id; 
     
  update p_rsf.rsf_data rd
  set data_sys_flags = 2  
  where rd.data_id = tmp.incorrect_data_id;  
  
  update p_rsf.rsf_data rd
  set data_sys_flags = 16,
      reporting_asof_date = tmp.correction_asof_date
  where rd.data_id = NEW.data_id;













--------------------------------------------------------------------------------------------------------------------------------------------
--MANUAL: Applied to reported data that it should not be overwritten and subsequently soft-delete the calculated data that did over write it.
--------------------------------------------------------------------------------------------------------------------------------------------
elseif (NEW.data_sys_flags & 4)=4
then


    if not exists(select * from p_rsf.rsf_data_current rdc
                  where rdc.data_id = new.data_id
                    and rdc.reporting_asof_date = new.check_asof_date) -- overwrite flag isnt current (didn't overwrite anything, evidently?).
       
    then
      -- an interesting scenario was discovered where
      --1a. Q1: Correct value as-of Q1 facility term was reported (system did nothing because agreement)
      --2a. Q2: Incorret value of same facility term that should have been for Q1 was reported
      --2b. Q2: Correct value was calculated and overwritten by system that equaled Q1 reported value.
      --2c. Q2: System saved correct value and replaced the 2a value in rsf_data_current and then inserted calculated 2c value. 
      --        But since 2c calculated value equals 1a reported value (and its not a flow data point), the triggers removed also the 2c value and the 
      --        Original 1a value and timeline persists as the "current value" -- which is totally correct and how it should work!
      --        But the FLAG for system calculator overwrote reported value was applied in Q2 on the Q1 value, with the message containing the value
      --        for the 2b value that was present at the time the overwrite was determined.
      --        The result is that a system calculator flag was applied on user reported data_id in the previous timeline.
      --        And now, when testing and telling it don't force calculate, instead is flagged onto historic Q1 manual data point.
      
      -- result: I modified the function for current check to make system calculator checks applied on reported data always auto-resolve.
      -- and here to only apply on actual calculated data that has actually overwritten a manual data point in the same timeline.
      
      raise exception 'This check cannot be tagged as a MANUALLY CALCULATED: the current value [%] was reported in %, but this check is for %',
      (NEW.data_value_unit),(select rd.reporting_asof_date from p_rsf.rsf_data rd where rd.data_id = NEW.data_id),(NEW.check_asof_date);    
    
    end if;

    if not exists(select * from p_rsf.rsf_data rd
                     inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
                     where rd.data_id = NEW.data_id
                       and rc.is_calculated_cohort is true) -- or its not flagged onto a calculated data point!
    then
    
      raise exception 'This check cannot be tagged as a MANUALLY CALCULATED: the current value [%] is actually a REPORTED value that has received a calculation flag (see check message for more details)',NEW.data_value_unit;
      
    end if;
                  
  select
    rd.reporting_asof_date, -- should almost always be same as-of data as the check.
    rd.data_id as correct_data_id,
    p_rsf.rsf_data_value_unit(rd.data_value,rd.data_unit) as correct_data_value_unit
  into tmp
  from p_rsf.rsf_data rd 
  inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
  where rd.rsf_pfcbl_id = NEW.rsf_pfcbl_id
    and rd.indicator_id = NEW.indicator_id
    and rd.reporting_asof_date <= NEW.check_asof_date
    and rd.data_id <> NEW.data_id --don't select me!
    --and rc.is_reported_cohort is true ... the overwrite flag will flag last reported andor _monitored_ value, which could be calculated
   order by
    rd.reporting_asof_date desc,
    rd.data_id desc
   limit 1;
  
  
  update p_rsf.rsf_data_checks rdc
  set check_status_comment = concat(NEW.check_status_comment,' [SYSTEM#',tmp.correct_data_id,'] ','System-calculated value ',
                                     NEW.data_value_unit,' marked invalid; and user-reported data ',
                                     tmp.correct_data_value_unit,' to be used as the true and correct value for ',
                                     tmp.reporting_asof_date),
      check_status = 'resolved',
      check_status_user_id = coalesce(NEW.check_status_user_id,
                                      (select account_id from p_rsf.view_account_info where is_system_account is true and users_name = 'RSF SYS Calculator')),
      data_correction_data_id = tmp.correct_data_id,
      data_correction_date = tmp.reporting_asof_date 
                                      
  where rdc.evaluation_id = NEW.evaluation_id; 
  

  -- will not trigger anything.
  delete from p_rsf.rsf_data_current rdc
  where rdc.data_id = NEW.data_id
     or rdc.data_id = tmp.correct_data_id;
     
  update p_rsf.rsf_data rd
  set data_sys_flags = 2   
  where rd.data_id = NEW.data_id;

  update p_rsf.rsf_data rd
  set data_sys_flags = 4  
  where rd.data_id = tmp.correct_data_id;  
  
  
  
  
  
  
  
  
  
  
  
  
  
---------------------------------------------------------------------------------------
--CALCULATE: Applied to reported data that it should be overwritten by system calculator
---------------------------------------------------------------------------------------
elseif (NEW.data_sys_flags & 8)=8
then

    if exists(select * from p_rsf.rsf_data rd
              inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
              where rd.data_id = NEW.data_id
                and rc.is_calculated_cohort is true)
    then
    
      raise exception 'This check cannot be tagged as CALCULATED: only manually reported data may be marked for system-overwrite (ie, user reported data)';    
    
    end if;
    
    if not exists(select * from p_rsf.view_rsf_setup_indicator_subscriptions sis
                  where sis.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                    and sis.indicator_id = NEW.indicator_id
                    and sis.is_calculated is true)
                    --and sis.formula_id is NOT NULL) -- formula_id is null vs is_calculated has to do with unit_fx_indicator_id being defined
    then
      raise exception 'This check cannot be tagged as CALCULATED because % is set as a reported (not calculated) metric in RSF Setup',
      (select indicator_name from p_rsf.indicators where indicator_id = NEW.indicator_id);    
    
    end if;                    
                    

    update p_rsf.rsf_data_checks rdc
    set check_status_comment = concat(NEW.check_status_comment,' [SYSTEM#',NEW.data_id,'] ','Marked that system-calculated value should be accepted for ',
                                         NEW.check_asof_date),
        check_status = 'resolved',
        check_status_user_id = coalesce(NEW.check_status_user_id,
                                        (select account_id from p_rsf.view_account_info where is_system_account is true and users_name = 'RSF SYS Calculator'))
    where rdc.evaluation_id = NEW.evaluation_id; 
  

    -- will not trigger anything.
    delete from p_rsf.rsf_data_current rdc
    where rdc.data_id = NEW.data_id;
  
    -- to ensure that recalculation is forced for this instance
    insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date)
    select NEW.rsf_pfcbl_id,NEW.indicator_id,NEW.check_asof_date
    on conflict do nothing;
    
    update p_rsf.rsf_data rd
    set data_sys_flags = 8 
    where rd.data_id = NEW.data_id; -- should re-insert this same value and trigger any related calculation evaluations.
    
    
    
    
    
    
    
    
-------------------------------------------------------------------------------------------------
--REMOVE TAG
-------------------------------------------------------------------------------------------------    
elseif TG_OP = 'UPDATE' AND NEW.data_sys_flags = 0
then

  -- REMOVE CORRECTION
  if (OLD.data_sys_flags & 16)=16
  then
    
    update p_rsf.rsf_data_checks rdc
    set check_status_comment = concat(NEW.check_status_comment,' [SYSTEM#',OLD.data_correction_data_id,'] ',
                                      ' Undo: marked as a correction. Reverting ',
                                      OLD.data_value_unit,' to ',OLD.check_asof_date),
        check_status = 'active',
        check_status_user_id = coalesce(NEW.check_status_user_id,
                                        (select account_id from p_rsf.view_account_info where is_system_account is true and users_name = 'RSF SYS Calculator')),
        data_correction_data_id = NULL,
        data_correction_date = NULL
    where rdc.evaluation_id = NEW.evaluation_id;  
    
    delete from p_rsf.rsf_data_current rdc
    where rdc.data_id in (NEW.data_id,OLD.data_correction_data_id);
    
    update p_rsf.rsf_data rd
    set reporting_asof_date = OLD.check_asof_date,
        data_sys_flags = 0
    where rd.data_id = NEW.data_id;
    
    update p_rsf.rsf_data rd
    set data_sys_flags = 0
    where rd.data_id = OLD.data_correction_data_id;
    
   
  -- REMOVE FORCE CALCULATE
  elseif (OLD.data_sys_flags & 8)=8
  then
  
    
    with calculated_data as (
      select
      crd.data_id
      from p_rsf.rsf_data rd
      inner join p_rsf.rsf_data crd on crd.rsf_pfcbl_id = rd.rsf_pfcbl_id
                                   and crd.indicator_id = rd.indicator_id
                                   and crd.reporting_asof_date in (rd.reporting_asof_date,OLD.check_asof_date)
      inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = crd.reporting_cohort_id
      where rd.data_id = OLD.data_id
        and rc.is_calculated_cohort is true
    )
    delete from p_rsf.rsf_data rd
    using calculated_data 
    where calculated_data.data_id = rd.data_id;

    delete from p_rsf.rsf_data_current rdc
    where rdc.data_id = NEW.data_id;
    
    update p_rsf.rsf_data rd
    set data_sys_flags = 0 
    where rd.data_id = NEW.data_id;
    
    update p_rsf.rsf_data_checks rdc
    set check_status_comment = concat(NEW.check_status_comment,' [SYSTEM#',OLD.data_id,'] ',
                                      ' Undo: marked as a use system calculation. Reverting to user-reported value ',
                                      OLD.data_value_unit),
        check_status = 'active',
        check_status_user_id = coalesce(NEW.check_status_user_id,
                                        (select account_id from p_rsf.view_account_info where is_system_account is true and users_name = 'RSF SYS Calculator')),
        data_correction_data_id = NULL,
        data_correction_date = NULL
    where rdc.evaluation_id = NEW.evaluation_id;  
    
  -- REMOVE USE MANUAL
  elseif (OLD.data_sys_flags & 4)=4
  then    
  
    delete from p_rsf.rsf_data_current rdc
    where rdc.data_id in (NEW.data_id,OLD.data_correction_data_id);
    
    update p_rsf.rsf_data rd
    set data_sys_flags = 0
    where rd.data_id in (NEW.data_id,OLD.data_correction_data_id);
    
    update p_rsf.rsf_data_checks rdc
    set check_status_comment = concat(NEW.check_status_comment,' [SYSTEM#',OLD.data_correction_data_id,'] ',
                                      ' Undo: marked as a use reported calculation. Reverting to system calculated value ',
                                      OLD.data_value_unit),
        check_status = 'active',
        check_status_user_id = coalesce(NEW.check_status_user_id,
                                        (select account_id from p_rsf.view_account_info where is_system_account is true and users_name = 'RSF SYS Calculator')),
        data_correction_data_id = NULL,
        data_correction_date = NULL
    where rdc.evaluation_id = NEW.evaluation_id;  
    
  end if;

    
end if;

  update p_rsf.rsf_data_checks rdc
  set check_data_id_is_current = x.is_current
  from (select p_rsf.rsf_data_check_is_current(v_data_id => NEW.data_id,
                                                       v_rsf_pfcbl_id => NEW.rsf_pfcbl_id,
                                                       v_indicator_id => NEW.indicator_id,
                                                       v_check_asof_date => NEW.check_asof_date) as is_current
  ) x
  where rdc.evaluation_id = NEW.evaluation_id
    and rdc.check_data_id_is_current is distinct from x.is_current;
    
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
-- Function structure for rsf_data_current_checks_set_current
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_current_checks_set_current"();
CREATE FUNCTION "p_rsf"."rsf_data_current_checks_set_current"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp;
BEGIN
 
  if (not exists(select true from modified)) then return NULL; end if;
  
  --raise notice '% p_rsf.rsf_data_current_checks_set_current(%) ',TG_OP,(select count(*) from modified);
  msg_time := clock_timestamp();
  
  update p_rsf.rsf_data_checks      
  set check_data_id_is_current = x.is_current
  from (                                                        
    select 
      chk.evaluation_id,
      is_current
    from p_rsf.rsf_data_checks chk
    inner join lateral p_rsf.rsf_data_check_is_current(v_data_id => chk.data_id,
                                                       v_rsf_pfcbl_id => chk.rsf_pfcbl_id,
                                                       v_indicator_id => chk.indicator_id,
                                                       v_check_asof_date => chk.check_asof_date) as is_current on true 
    where exists(select * from modified
                 where chk.rsf_pfcbl_id = modified.rsf_pfcbl_id
                   and chk.indicator_id = modified.indicator_id
                   and chk.check_asof_date >= modified.reporting_asof_date)
  ) x
  where x.evaluation_id = rsf_data_checks.evaluation_id
    and x.is_current is distinct from rsf_data_checks.check_data_id_is_current;


	raise notice 'TD% rsf_data_current_checks_set_current(%) % COMPLETED in  %',
  pg_trigger_depth(),
	(select count(*) from modified),
  TG_OP,  
	(clock_timestamp()-msg_time);
                   
  return NULL;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_current_fx_modified
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_current_fx_modified"();
CREATE FUNCTION "p_rsf"."rsf_data_current_fx_modified"()
  RETURNS "pg_catalog"."trigger" AS $BODY$

DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN


	
	
  -- Table p_rsf.rsf_data_current_fx
  -- Contains a reference to ALL fx rate(s) used by this entity/indicator/date [by the system calculator to calculate OR validate the calculation]
  -- Note that many calculations, eg, aggregating calculations, may use multiple FX rates.  Hence the separate table.
  -- Where each fx_data_id is a reference to a currency_ratio value in rsf_data_current (that may or may not be the same as the reporting_asof_date,
  -- which is the calculation/reporting/validation date for this entry; but for unchanged or pegged fx rates, we may draw on an historicaly reported fx rate)
  -- This trigger sees that the fx figure to calculate this metric has changed! Either deleted or updated.  Therefore, the calculation itself is questionable 
  -- and it should be re-evaluated. 
  -- This trigger does that.

  if not exists(select true from modified_fx)
  then
    return null;
  end if;

  msg_time := clock_timestamp();
  
	insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
																										 indicator_id,
																										 calculation_asof_date,
                                                     rsf_pf_id,
                                                     formula_calculation_rank)
  select 
    calc.rsf_pfcbl_id,
    calc.indicator_id,
    calc.reporting_asof_date,
    calc.rsf_pf_id,
    coalesce(indf.formula_calculation_rank,0) as formula_calculation_rank
  from (select distinct
    mx.rsf_pfcbl_id,
    mx.indicator_id,
    mx.reporting_asof_date,
    ids.rsf_pf_id
    from modified_fx mx
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = mx.rsf_pfcbl_id  
    where mx.reporting_asof_date >= ids.created_in_reporting_asof_date  
  ) calc
  inner join p_rsf.rsf_setup_indicators rsi on rsi.rsf_pfcbl_id = calc.rsf_pf_id
                                           and rsi.indicator_id = calc.indicator_id
  left join p_rsf.indicator_formulas indf on indf.formula_id = rsi.formula_id  
  where rsi.is_subscribed is true
  on conflict do nothing; 
  -- also not checking if it's a unit_fx_indicator_id vs formula, etc... Again, if it's in there it's because calculator used fx to calculate it
  -- and so presumably may do so again.
  -- checking creation date as it would be a problem to calculate before existence
  -- not checking deactivation date or fx method because this entry already exists at this reporting date.  So presumably it was there intentionally/correctly
  -- and re-validating that correctness adds little value
  
  /*
  -- If the OLD fx entry was deleted or modified
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
	*/					
  
  -- delete will cascade already
  -- but update wont and so remove these stale values to be re-inserted on calculation just triggered
  if TG_OP = 'UPDATE' then
    delete from p_rsf.rsf_data_current_fx dfx
    using modified_fx mx
    where mx.data_id = dfx.fx_data_id;
  end if;
	return NULL;
  
  
 	raise info ' - rsf_data_current_fx_modified(%) re-evaluating: % calculations in %',
  TG_OP,
	(select count(*) from modified_fx),
	(select clock_timestamp()-msg_time);

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
	
--raise info 'rsf_data_current_lcu_modified(%)',TG_OP;
--msg_time := clock_timestamp();

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
  
  
    
	  if exists(select true 
              from p_rsf.rsf_pfcbl_ids ids
              where ids.rsf_pfcbl_id > 0
                and not exists(select true
                               from p_rsf.rsf_data_current_lcu lcu
                               where lcu.for_rsf_pfcbl_id = ids.rsf_pfcbl_id
                                 and lcu.reporting_asof_date = ids.created_in_reporting_asof_date))
	  then		
    
			raise notice 'Delete from rsf_data_current_lcu resulted in entity not having a defined local currency as of its created_in_reporting_asof_date';
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
	
	--raise info 'rsf_data_current_lcu_modified _temp_modified_lcu=% and modified_lcu size=% in %',
	--(select count(*) from _temp_modified_lcu),(select count(*) from modified_lcu),(select clock_timestamp()-msg_time);
	
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
	
	with updates as ( 
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
	
	
	--raise info 'rsf_data_current_lcu_modified currencies modified in %',
	--(select clock_timestamp()-msg_time);
	--msg_time := clock_timestamp();


  -- recalculate all currency calculations or those with a currency parameter
	-- that are affected by the deletion of a lcu data point
  with quasi_currencies as (
    select ind.indicator_id,ind.unit_fx_indicator_id,ind.pfcbl_rank
    from p_rsf.indicators ind
    where ind.data_type = 'currency'
    or exists(select true
				      from p_rsf.indicator_formula_parameters ifp
					  	where ifp.indicator_id = ind.indicator_id
						   and ifp.parameter_data_type = 'currency')
  )
	insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,
	                                                   indicator_id,
																										 calculation_asof_date,
                                                     rsf_pf_id,
                                                     formula_calculation_rank)
	select 
		ids.rsf_pfcbl_id,
		rsi.indicator_id,
		lcu.reporting_asof_date,
    ids.rsf_pf_id,
    coalesce(indf.formula_calculation_rank,0) as formula_calculation_rank
	from _temp_modified_lcu lcu
  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = lcu.for_rsf_pfcbl_id
  inner join quasi_currencies qc on qc.pfcbl_rank = ids.pfcbl_category_rank
  inner join p_rsf.rsf_setup_indicators rsi on rsi.rsf_pfcbl_id = ids.rsf_pf_id                                           
                                           and rsi.indicator_id = qc.indicator_id
  left join p_rsf.indicator_formulas indf on indf.formula_id = rsi.formula_id
	where rsi.is_subscribed is true
    and (rsi.formula_id is not null or qc.unit_fx_indicator_id is not null)
    and lcu.reporting_asof_date >= ids.created_in_reporting_asof_date 
  on conflict 
	do nothing;
  
  
		
 	raise info '  - rsf_data_current_lcu_modified _temp_modified_lcu=% and modified_lcu size=% in %',
	(select count(*) from _temp_modified_lcu),
  (select count(*) from modified_lcu),
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
DECLARE unit_fx_method text default NULL;
BEGIN
  -- function reviews current versus previous data values: if they're the same, and if it is NOT a LEGITIMATE flow data point
	-- then delete the current value from rsf_data_current
  --raise warning 'rsf_data_current_modified_unchanged %',NEW.data_id;
  
  -- MARCH 2026: historical corrections need to ensure their future (incorrect) data are removed.  And flags related to future reporting are 
  select
	 future.data_id
	 into redundant_data_id
	from
	 (select rdc.data_value,rdc.data_unit,rdc.data_id
		from p_rsf.rsf_data_current rdc
		where rdc.rsf_pfcbl_id = NEW.rsf_pfcbl_id
			and rdc.indicator_id = NEW.indicator_id
			and rdc.reporting_asof_date > NEW.reporting_asof_date
		order by 
			rdc.reporting_asof_date asc -- ensure we get the first next one, not "desc" as per usual.
		limit 1
	 ) future
	where future.data_value is not distinct from NEW.data_value
	  and future.data_unit is not distinct from NEW.data_unit;
  
  if (redundant_data_id is not NULL)
  then
    if (exists(select true from p_rsf.indicators ind 
               where ind.indicator_id = NEW.indicator_id
                 and ind.is_periodic_or_flow_reporting is true))
    then 
    
      -- even periodic flow data shouldn't permit consecutive empty entries;
      if (NEW.data_value is NULL and NEW.data_unit is NULL)
      then
      
        delete from p_rsf.rsf_data_current rdc where rdc.data_id = redundant_data_id;
        --update p_rsf.rsf_data_checks rdc set check_data_id_is_current = false; -- tigger will reset regardless / moved to rsf_data_current statement trigger
      
      end if;
    else 
      delete from p_rsf.rsf_data_current rdc where rdc.data_id = redundant_data_id;
      --update p_rsf.rsf_data_checks rdc set check_data_id_is_current = false; -- tigger will reset regardless / moved to rsf_data_current statement trigger
    end if;
  
    
    
  end if;
  
  redundant_data_id := NULL;  
  
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
			coalesce(sis.is_periodic_or_flow_reporting,false) OR coalesce(sis.indicator_sys_category = 'entity_reporting',false),
			coalesce(sis.unit_fx_method,'none') 
			into
			is_calculated,
			is_periodic,
			unit_fx_method
		from p_rsf.view_rsf_setup_indicator_subscriptions sis 
    -- important to return either/both subscribed, unsubscribed -- although should at least be auto-subscribed if we are at this point
    -- due to auto-subscriptions set in previous trigger
		
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
			else 
        -- is_calculated = true
			  -- if its a fx-triggered calculation, then it will be triggered by entity reporting.
				-- the main risk here to avoid is that a user submits a user-calculated update in Excel that shouldn't be re-calculated
				-- but they're submitting a change on a periodic data point due to maybe an excel template adjustment and that re-calculation
				-- shouldn't have happened in the first place.
				if (unit_fx_method = 'fx')
				then
					redundant_data_id := NULL;
				else 
          -- It's unchanged
          -- But if it's a calculated metric and it has parameters reported, then it can reasonably reundantly (re)reported.
          -- Previously a special parameter trigger check, which are very expensive to query.  
          -- But we've already done this!!
          -- But if it did have a trigger or other legitimate basis for having been computed, it should be in the evaluations queue (or already evaluated)
          select exists(select true 
                        from p_rsf.rsf_data_calculation_evaluations dce
                        where dce.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                          and dce.indicator_id = NEW.indicator_id
                          and dce.calculation_asof_date = NEW.reporting_asof_date)
                 OR
                 
                 exists(select true
                        from p_rsf.rsf_data_calculation_validations dcv
                        where dcv.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                          and dcv.indicator_id = NEW.indicator_id
                          and dcv.calculation_asof_date = NEW.reporting_asof_date)
          into is_periodic;
          
          -- removed because it's so expensive for a row trigger, even if this is an uncommon event.
          -- intention was to get the entity's formula's parameters and see if any had an entry in rsf_data for this reporting_asof_date
          -- that is: did any parameters trigger this calculation?  If so, we let it pass.  If not
          /*
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
					select exists(select true from p_rsf.rsf_data rd
												where exists(select true from params
																		 where params.parameter_rsf_pfcbl_id = rd.rsf_pfcbl_id
																			 and params.parameter_indicator_id = rd.indicator_id
																			 and rd.reporting_asof_date = NEW.reporting_asof_date))::bool
					into is_periodic; -- recycling variable
				  */
          
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
-- Function structure for rsf_data_current_names_and_ids_restoring
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_current_names_and_ids_restoring"();
CREATE FUNCTION "p_rsf"."rsf_data_current_names_and_ids_restoring"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN


 if (exists(select true from p_rsf.rsf_setup_archive rsa
            where rsa.sys_name = NEW.sys_name
              and rsa.is_restored is distinct from true
              and rsa.is_disabled is distinct from true))
 then                 
  perform p_rsf.function_rsf_setup_restore(v_sys_name => NEW.sys_name);
 end if;
 
 return NEW;
	
END;
$BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_current_nids_set_sysname
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_current_nids_set_sysname"();
CREATE FUNCTION "p_rsf"."rsf_data_current_nids_set_sysname"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp;
BEGIN

  
  if not exists(select true from modified) then return NULL; end if;

  msg_time := clock_timestamp();
  if (TG_OP = 'INSERT')
  then
  
    with sys_names as (
      select 
        ids.rsf_pfcbl_id,
        mids.reporting_asof_date,
        array_to_string(array_agg(p_nai.pfcbl_name order by parent.rsf_pfcbl_id asc),' > ') as sys_name
      from modified mids
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = mids.rsf_pfcbl_id    
      --inner join lateral (select unnest((array[ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id,ids.rsf_loan_id])) as rsf_pfcbl_id) as parent on true
      inner join lateral (values (rsf_program_id),
                                 (rsf_facility_id),
                                 (rsf_client_id),
                                 (rsf_borrower_id),
                                 (rsf_loan_id) ) as parent(rsf_pfcbl_id) on parent.rsf_pfcbl_id is not null
      left join lateral(select nids.pfcbl_name
                        from p_rsf.rsf_data_current_names_and_ids nids 
                        where nids.rsf_pfcbl_id = parent.rsf_pfcbl_id
                          and nids.reporting_asof_date <= mids.reporting_asof_date::date 
                        order by nids.reporting_asof_date desc
                        limit 1) as p_nai on true
      group by ids.rsf_pfcbl_id,mids.reporting_asof_date
    )
    update p_rsf.rsf_data_current_names_and_ids cni
    set sys_name = sn.sys_name
    from sys_names sn
    where sn.rsf_pfcbl_id = cni.rsf_pfcbl_id
      and sn.reporting_asof_date = cni.reporting_asof_date;
  
  else 
  
    with updates as (     
      select 
        modified.rsf_pfcbl_id,
        modified.reporting_asof_date,
        modified.pfcbl_name
      from modified
      
      EXCEPT
      
      select
        removed.rsf_pfcbl_id,
        removed.reporting_asof_date,
        removed.pfcbl_name
      from removed
    ),
    sys_names as (
      select 
      ids.rsf_pfcbl_id,
      mids.reporting_asof_date,
      array_to_string(array_agg(p_nai.pfcbl_name order by parent.rsf_pfcbl_id asc),' > ') as sys_name
      from updates mids
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = mids.rsf_pfcbl_id    
      --inner join lateral (select unnest((array[ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id,ids.rsf_loan_id])) as rsf_pfcbl_id) as parent on true
      inner join lateral (values (rsf_program_id),
                                 (rsf_facility_id),
                                 (rsf_client_id),
                                 (rsf_borrower_id),
                                 (rsf_loan_id) ) as parent(rsf_pfcbl_id) on parent.rsf_pfcbl_id is not null
      left join lateral(select nids.pfcbl_name
                        from p_rsf.rsf_data_current_names_and_ids nids 
                        where nids.rsf_pfcbl_id = parent.rsf_pfcbl_id
                          and nids.reporting_asof_date <= mids.reporting_asof_date::date 
                        order by nids.reporting_asof_date desc
                        limit 1) as p_nai on true
      group by ids.rsf_pfcbl_id,mids.reporting_asof_date
    )
    update p_rsf.rsf_data_current_names_and_ids cni
    set sys_name = sn.sys_name
    from sys_names sn
    where sn.rsf_pfcbl_id = cni.rsf_pfcbl_id
      and sn.reporting_asof_date = cni.reporting_asof_date;
  end if;
  

    
    
  raise notice 'TD% rsf_data_current_nids_set_sysname(%) % COMPLETED in %',
	pg_trigger_depth(),
  (select count(*) from modified),
  TG_OP,  
	(clock_timestamp()-msg_time);


	return NULL;
	
END;
$BODY$
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
-- Function structure for rsf_data_inserted_data_integrity
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_inserted_data_integrity"();
CREATE FUNCTION "p_rsf"."rsf_data_inserted_data_integrity"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
declare cohort_counts int default null;
BEGIN


--return null;

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
            inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = tud.reporting_cohort_id
						inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tud.rsf_pfcbl_id
						where rc.reporting_rsf_pfcbl_id <> coalesce(ids.rsf_facility_id,ids.rsf_program_id)
              and rc.reporting_type > 0)			
  then 																	 
		raise exception 'Error in p_rsf.rsf_data_inserted_data_integrity(): rc.reporting_rsf_pfcbl_id must report facility data+ under rsf_facility_id and program data under rsf_program_id (unless cohort reporting type=0, system setup data)';
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
-- Function structure for rsf_data_modified_calculations
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_modified_calculations"();
CREATE FUNCTION "p_rsf"."rsf_data_modified_calculations"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
--DECLARE trigger_is_calculated_cohort bool default false;
BEGIN

  if (not exists(select * from modified_data))
	then 
		return null;
	end if;
	
  msg_time := clock_timestamp();
  
  with cohorts as materialized (
    select 
    rc.reporting_rsf_pfcbl_id as rsf_pf_id,
    min(rc.reporting_asof_date) as evaluation_asof_date,
    case when count(distinct rc.reporting_cohort_id) > 1 
         then min(rc.reporting_calculation_rank)
         else 0 
    end as trigger_calculation_rank
    from p_rsf.reporting_cohorts rc
    inner join (
      select distinct md.reporting_cohort_id 
      from modified_data md
    ) cids on cids.reporting_cohort_id = rc.reporting_cohort_id
    WHERE TG_OP <> 'DELETE'
    group by
    rc.reporting_rsf_pfcbl_id
    
    UNION ALL
    
    select 
    ids.rsf_pf_id,
    min(md.reporting_asof_date) as evaluation_date,
    0 as trigger_calculation_rank
    from modified_data md
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = md.rsf_pfcbl_id
    WHERE TG_OP = 'DELETE'
    group by ids.rsf_pf_id
  )
  insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date,rsf_pf_id,formula_calculation_rank)
  select distinct
    calc.calculate_rsf_pfcbl_id,
    calc.calculate_indicator_id,
    calc.calculate_asof_date,
    calc.to_rsf_pf_id,
    calc.to_formula_calculation_rank
  from cohorts
  cross join lateral (
    select
      cer.calculate_rsf_pfcbl_id,
      cer.calculate_indicator_id,
      cer.calculate_asof_date,
      cer.to_rsf_pf_id,
      cer.to_formula_calculation_rank
    from p_rsf.view_rsf_pf_calculation_evaluations_required cer
    where cer.from_rsf_pf_id = cohorts.rsf_pf_id
      and cer.from_reporting_asof_date = cohorts.evaluation_asof_date -- should it be >= instead of = ?
      and cer.from_reporting_calculation_rank >= cohorts.trigger_calculation_rank
    offset 0 -- offset 0 is a "hack" for the query planner to ensure the cohort is fully collapsed in-line; which it doesn't do, even with "materialized"
  ) as calc
  on conflict do nothing;
  
  
	raise info 'TG% % rsf_data_modified_calculations calculated(%) in %',
	pg_trigger_depth(),  
  lower(TG_OP),	
  (select count(*) from modified_data),  
  --trigger_is_calculated_cohort,
  (clock_timestamp()-msg_time);

  --drop table _modified_data;
  --drop table _calculate;
	return NULL;
  
  /*
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
  */

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
                            
  /*
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
  */
  
  with new_reporting as (
    select
    parents.rsf_pfcbl_id,
    pind.indicator_id,
    reporting.reporting_asof_date
    from (
      select distinct
      mcd.rsf_pfcbl_id,
      mcd.reporting_asof_date
      from modified_data mcd
      inner join p_rsf.indicators ind on ind.indicator_id = mcd.indicator_id
      where ind.indicator_sys_category = 'entity_reporting'
    ) reporting
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = reporting.rsf_pfcbl_id
    inner join lateral (values (rsf_loan_id,5),
                               (rsf_borrower_id,4),
                               (rsf_client_id,3),
                               (rsf_facility_id,2),
                               (rsf_program_id,1)) as parents(rsf_pfcbl_id,pfcbl_rank) on parents.rsf_pfcbl_id is not null
    inner join p_rsf.indicators pind on pind.pfcbl_rank = parents.pfcbl_rank
    where pind.indicator_sys_category = 'entity_reporting'    
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
    /*  
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
    */
    from (
      select distinct
      ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id, -- newly reported ID
      reporting.reporting_asof_date
      from (select distinct
            mcd.rsf_pfcbl_id,
            mcd.reporting_asof_date
            from modified_data mcd
      ) reporting
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = reporting.rsf_pfcbl_id
                                                       and ft.pfcbl_hierarchy = 'parent' -- parent entity (ie, not itself)
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
	where exists(select true
               from p_rsf.rsf_pfcbl_ids ids
               where ids.rsf_pfcbl_id = ep.to_check_rsf_pfcbl_id
                 and ids.created_in_reporting_asof_date <= ep.reporting_asof_date
                 and (ids.deactivated_in_reporting_asof_date is NULL
                      or
                      ids.deactivated_in_reporting_asof_date >= ep.reporting_asof_date))
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
			chk.check_asof_date,
			chk.check_formula_id
		from _check chk		
		inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = chk.rsf_pfcbl_id
		inner join p_rsf.view_rsf_setup_check_subscriptions scs on scs.rsf_pfcbl_id = ids.rsf_pfcbl_id
		                                                       and scs.check_formula_id = chk.check_formula_id
		where (chk.check_asof_date between ids.created_in_reporting_asof_date and coalesce(ids.deactivated_in_reporting_asof_date,chk.check_asof_date))
      and exists(select true
                 from p_rsf.reporting_cohorts rc
                 where rc.reporting_rsf_pfcbl_id = ids.rsf_pf_id
                   and rc.reporting_asof_date = chk.check_asof_date)
		  and scs.is_subscribed is true
		on conflict(rsf_pfcbl_id,check_asof_date,check_formula_id)
    do nothing;



	raise info 'rsf_data_current_%_checks inserted check evaluations %',
	(upper(TG_OP)),
	(select clock_timestamp()-msg_time);

  drop table _check;
	return NULL;
END; $BODY$
  LANGUAGE plpgsql VOLATILE SECURITY DEFINER
  COST 100;

-- ----------------------------
-- Function structure for rsf_data_modified_data_current
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_data_modified_data_current"();
CREATE FUNCTION "p_rsf"."rsf_data_modified_data_current"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN
	
	if (not exists(select * from modified_rsf_data)) then 
		return NULL;
	end if;
	

	
	
	raise notice 'TD% rsf_data_modified_data_current(%) for % on p_rsf.rsf_data',
	pg_trigger_depth(),
  (select count(*) from modified_rsf_data),
  TG_OP;
	
	msg_time := clock_timestamp(); 
  -- SETUP:1
  -- Ensure reported indicators are subscribed at the rsf_pf_id level
  -- but only relevant for inserts
  if (TG_OP = 'INSERT') then
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
        coalesce(ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id, -- this is really rsf_pf_id, but pre-dated that nomenclature and retained for simplicity.
        ird.indicator_id,
        ird.reporting_cohort_id
      from modified_rsf_data ird
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ird.rsf_pfcbl_id
    ) reported
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = reported.rsf_pfcbl_id
    left join p_rsf.indicator_formulas indf on indf.indicator_id = reported.indicator_id
                                           and indf.is_primary_default = true
    where not exists(select true from p_rsf.rsf_setup_indicators pfi 
                     where pfi.rsf_pfcbl_id = reported.rsf_pfcbl_id 
                       and pfi.indicator_id = reported.indicator_id)
    on conflict -- if it's already there, subscribed or unsubscribed at this entity's level, do nothing.
    do nothing;

    raise info ' - rsf_data_modified_data_current: (SETUP) completed insert into p_rsf.rsf_setup_indicators in %',
    (select clock_timestamp()-msg_time); 
    
    
  end if;
	
  
  ---------------------------------------------------------------------------------------------
  -- LCU:2
  -- Ensure rsf_data_current_lcu is complete: rsf_data_current requires this as a pre-requisite 
  msg_time := clock_timestamp();
  if (exists(select true 
             from  modified_rsf_data mrd
             inner join p_rsf.indicators ind on ind.indicator_id = mrd.indicator_id
             where ind.indicator_sys_category in ('entity_local_currency_unit','entity_currency_unit')))   then
  
    with lcu_reported as (
      select 
        mrd.rsf_pfcbl_id,
        ind.indicator_sys_category = 'entity_local_currency_unit' as is_inherited
      from modified_rsf_data mrd
      inner join p_rsf.indicators ind on ind.indicator_id = mrd.indicator_id
      where ind.indicator_sys_category in ('entity_currency_unit','entity_local_currency_unit') 
    ),
    lcu_ids as (
      select 
        ids.rsf_pfcbl_id,
        ids.pfcbl_category_rank,
        rsf_gpfcbl_family,
        ids.created_in_reporting_asof_date
      from lcu_reported lcur
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = lcur.rsf_pfcbl_id
      where lcur.is_inherited is false
      
      union all
      
      select 
        ids.rsf_pfcbl_id,
        ids.pfcbl_category_rank,
        rsf_gpfcbl_family,
        ids.created_in_reporting_asof_date
      from lcu_reported lcur
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = lcur.rsf_pfcbl_id
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
      where lcur.is_inherited is true
        and ft.pfcbl_hierarchy <> 'parent'
    )
    insert into p_rsf.rsf_data_current_lcu(lcu_unit_data_id,
                                           for_rsf_pfcbl_id,
                                           reporting_asof_date,
                                           data_unit_value,
                                           data_id_pfcbl_rank,
                                           is_defined_lcu)
                                           
    select distinct on (lcu.for_rsf_pfcbl_id,lcu.reporting_asof_date)
      lcu.lcu_unit_data_id,
      lcu.for_rsf_pfcbl_id,
      lcu.reporting_asof_date,
      lcu.data_unit_value,
      lcu.data_id_pfcbl_rank,
      lcu.is_defined_lcu
    from (
      select 
      rd.data_id as lcu_unit_data_id,
      ids.rsf_pfcbl_id as for_rsf_pfcbl_id,
      
      -- if its defined, no data can be reported before its own creation date since defined metrics are always <-> pfcbl_rank for indicators and ids
      -- but if its inherited, it can (and should) inherit the earlier known value, but inherit it asof its own creation date.
      greatest(rd.reporting_asof_date,ids.created_in_reporting_asof_date) as reporting_asof_date,
      rd.data_value as data_unit_value,
      ind.pfcbl_rank as data_id_pfcbl_rank,
      ind.indicator_sys_category = 'entity_currency_unit' as is_defined_lcu,  
      min(greatest(rd.reporting_asof_date,ids.created_in_reporting_asof_date)) 
        filter(where ind.indicator_sys_category = 'entity_currency_unit')
        over(partition by ids.rsf_pfcbl_id) 
      as use_defined_unit_asof_date
    from lcu_ids ids 
    inner join p_rsf.indicators ind on ind.indicator_sys_category in ('entity_currency_unit','entity_local_currency_unit') 
                                                                 and ind.pfcbl_rank <= ids.pfcbl_category_rank
    cross join lateral (values (ids.rsf_gpfcbl_family[1+ind.pfcbl_rank])) as lcuids(rsf_pfcbl_id)

    -- updated to operate on rsf_data instead of rsf_data_current so that we can use it for setting currency units as part of insert into rsf_data_current
    -- no filter on rd.reporting_asof_date: this will insert/update all values that we know about regardless of the reporting timeline triggered.
    inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = lcuids.rsf_pfcbl_id
                                and rd.indicator_id = ind.indicator_id
    where (
            (ind.indicator_sys_category = 'entity_currency_unit' and ind.pfcbl_rank = ids.pfcbl_category_rank)
            or
            (ind.indicator_sys_category = 'entity_local_currency_unit' and ind.pfcbl_rank <= ids.pfcbl_category_rank)
      )      
      and rd.data_sys_flags&2 is distinct from 2 -- don't include "soft deleted" data
      and rd.data_value is not null -- dont unclude undefiend values
      and rd.data_value <> 'LCU'    -- dont include generic LCU, ie, undefined values
    ) as lcu
    where ( --true or
            (lcu.is_defined_lcu is true)  -- if its a defined value, always include it and prefer it.
            or 
            (lcu.use_defined_unit_asof_date is NULL)  -- if NULL means has never reported any defined units (and so will always inherit)
            or
            (lcu.reporting_asof_date < lcu.use_defined_unit_asof_date)  -- has reported both defined and has inherited values: only inherit before first defined value is reported.
          )

    order by 
      lcu.for_rsf_pfcbl_id, -- unique by entity 
      lcu.reporting_asof_date, -- unique by reporting date (or if its before it was created, use created date due to greatest above())
      lcu.is_defined_lcu desc, -- prioritize entity defined currency over generic local currency unity (parent)
      lcu.data_id_pfcbl_rank desc, -- prioritize the "localist" ie, facility over program where there are multiple inherited values
      lcu.lcu_unit_data_id desc    -- prioritize the most current data point -- should align with rsf_data_current
    on conflict(for_rsf_pfcbl_id,reporting_asof_date)
    do update
    set 
      lcu_unit_data_id = excluded.lcu_unit_data_id,
      data_unit_value = excluded.data_unit_value,
      data_id_pfcbl_rank = excluded.data_id_pfcbl_rank,
      is_defined_lcu = excluded.is_defined_lcu;

      raise info ' - rsf_data_modified_data_current: (LCU) completed insert into rsf_data_current_lcu in %',
      (select clock_timestamp()-msg_time); 
     

  end if;
	
  
  ---------------------------------------------------------------------------------------------
  -- NIDS:3
  -- Ensure rsf_data_current_names_and_ids is complete: not strictly a pre-requisite, but sys_name is essential and ensure on any relevant movement.
  msg_time := clock_timestamp();
  if (exists(select true 
              from  modified_rsf_data mrd
              inner join p_rsf.indicators ind on ind.indicator_id = mrd.indicator_id
              where ind.indicator_sys_category in ('id','rank_id','name','nickname','tranche_id'))) then  
              
    with nids_times as (
      select 
        nidrdc.rsf_pfcbl_id,  
        nidrdc.reporting_asof_date,
        ind.pfcbl_rank as data_category_rank      
      from p_rsf.indicators ind
      inner join p_rsf.rsf_data nidrdc on nidrdc.indicator_id = ind.indicator_id
      where ind.indicator_sys_category in ('id','rank_id','name','nickname','tranche_id')
        and nidrdc.data_value is not NULL
        and nidrdc.data_sys_flags&2 is distinct from 2        
        and exists(select true 
                   from modified_rsf_data mrd
                   where mrd.rsf_pfcbl_id = nidrdc.rsf_pfcbl_id
                     and mrd.indicator_id = ind.indicator_id)                 
      group by 
        nidrdc.rsf_pfcbl_id,
        nidrdc.reporting_asof_date,
        ind.pfcbl_rank
    )
    -- and insert/update will trigger an update of sys_name
    insert into p_rsf.rsf_data_current_names_and_ids(rsf_pfcbl_id,reporting_asof_date,"id",rank_id,tranche_id,"name",nickname,pfcbl_category,pfcbl_name,data_cohort_id)	
    select 
      updates.rsf_pfcbl_id,
      updates.reporting_asof_date,      
      --leading zeros shoud be trimmed at the application layer.
      trim(max(regexp_replace(cd.data_value,'^(.*)#[[:digit:]]+$','\1','g')) filter (where ind.indicator_sys_category = 'id')) as "id",
      max(cd.data_value) filter (where ind.indicator_sys_category = 'rank_id') as rank_id,
      max(cd.data_value) filter (where ind.indicator_sys_category = 'tranche_id') as tranche_id,
      --max(cd.data_value) filter (where ind.indicator_sys_category = 'name') as "name",
      trim(max(regexp_replace(regexp_replace(cd.data_value,'[^A-Za-z0-9[:space:]''&.-]',' ','g'),'[[:space:]]{2,}',' ','g')) filter (where ind.indicator_sys_category = 'name')) as "name",
      max(cd.data_value) filter (where ind.indicator_sys_category = 'nickname') as "nickname",
      ind.data_category,      
      -- if no ID data has been submitted, then the pfcbl_name must be null, else unique index conflicts can arise from concat(ids.pfcbl_category...
      case when max(cd.data_value) filter (where ind.indicator_sys_category = 'rank_id' AND ind.data_category = 'loan') is NULL
            and max(cd.data_value) filter (where ind.indicator_sys_category = 'id') is NULL
            and max(cd.data_value) filter (where ind.indicator_sys_category = 'name') is NULL
            -- this moots having a sys name since rsf_pfcbl_id is a sequence. On the other hand, this should get promptly overwritten
            -- and only exist briefly for a newly created entity
            then concat(ind.data_category || ':SYSID',updates.rsf_pfcbl_id)                                                                              
           else 
      concat(ind.data_category || ':',
             coalesce('RANK' || max(cd.data_value) filter (where ind.indicator_sys_category = 'rank_id' AND ind.data_category = 'loan'),
                                trim(max(regexp_replace(regexp_replace(cd.data_value,'[^A-Za-z0-9[:space:]''&.-]',' ','g'),'[[:space:]]{2,}',' ','g')) 
                                filter (where ind.indicator_sys_category = 'name'))),
             ' (' || trim(max(regexp_replace(cd.data_value,'^(.*)#[[:digit:]]+$','\1','g')) filter (where ind.indicator_sys_category = 'id') || ')')) 
      end as pfcbl_name,
      max(cd.reporting_cohort_id) as data_cohort_id
    from 
    nids_times as updates
    inner join p_rsf.indicators ind on ind.pfcbl_rank = data_category_rank
                                   and ind.indicator_sys_category in ('id','rank_id','name','nickname','tranche_id')
    inner join lateral(select 
                        rd.data_value,
                        rd.data_id,
                        rd.reporting_cohort_id
                       from p_rsf.rsf_data rd
                       where rd.rsf_pfcbl_id = updates.rsf_pfcbl_id
                         and rd.indicator_id = ind.indicator_id
                         and rd.reporting_asof_date <= updates.reporting_asof_date
                         and rd.data_value is not NULL
                         and rd.data_sys_flags&2 is distinct from 2
                       order by 
                         rd.reporting_asof_date desc,
                         rd.data_id desc
                       limit 1) as cd on true
    group by
      updates.rsf_pfcbl_id,
      updates.reporting_asof_date,
      ind.data_category
    on conflict(rsf_pfcbl_id,reporting_asof_date)
    do update
    set "id" = excluded."id",
        rank_id = excluded.rank_id,
        "name" = excluded."name",
        nickname = excluded.nickname,
        pfcbl_category = excluded.pfcbl_category, -- shouldn't ever change, but also no fk
        pfcbl_name = excluded.pfcbl_name,
        data_cohort_id = excluded.data_cohort_id;  -- to assert fk to delete stale entries (otherwise if no data exists, row will persist and/or be all nulls)   
  
    raise info ' - rsf_data_modified_data_current: (NIDS) completed insert into rsf_data_current_names_and_ids in %',
    (select clock_timestamp()-msg_time); 
    

  end if;
  
  
  
  ----------------------------------------------------------------------------           
  -- ACTIVE:4
  -- Ensure rsf_pfcbl_ids deactivated date is updated: downstream calculation triggers can be affected by this as a pre-requisite  
  msg_time := clock_timestamp();     
  if (exists(select true 
              from  modified_rsf_data mrd
              inner join p_rsf.indicators ind on ind.indicator_id = mrd.indicator_id
              where ind.indicator_sys_category = 'is_active')) then  

    -- this query is seemingly overly complicated...
    -- but can arise where users update status in-situ and "active" changes is to closed in multiple timelines (which rsf_data_current will clean-up) but timeline of when it closed 
    -- will increment from rsf_data's viewpoint.
    -- Also issues where facilities are re-activated and re-closed, generating multiple active/closed timestamps (which can't shouldn't happen, yet does)
    -- and even issues were clients re-report data using new terms, like instead of Yes/No on active status reporting, change to "disactivated" or "non" and the newly reported data is
    -- received as a change that then is re-resolved and also affects the timeline of when the changed happen.
    -- so therefore seek currestest data, for the earliested 'false' is_active value following the latest a 'true' is_active value
    -- (or if it was never reported as active or updated to never have been active
    with activity_updates as (
      select distinct 
        mrd.rsf_pfcbl_id,ind.indicator_id
      from modified_rsf_data mrd
      inner join p_rsf.indicators ind on ind.indicator_id = mrd.indicator_id
        and ind.indicator_sys_category = 'is_active'
    ),
    status_change as (
      select 
        updates.rsf_pfcbl_id,
        deactivated.reporting_asof_date as deactivated_in_reporting_asof_date,
        deactivated.reporting_cohort_id as deactivated_by_reporting_cohort_id
      from p_rsf.rsf_pfcbl_ids ids
      inner join activity_updates updates on updates.rsf_pfcbl_id = ids.rsf_pfcbl_id
      left join (
        select distinct on (closed.rsf_pfcbl_id)
        closed.rsf_pfcbl_id,
        closed.reporting_asof_date,
        closed.reporting_cohort_id
          from (
          select 
            currentest.rsf_pfcbl_id,
            currentest.reporting_asof_date, 
            currentest.reporting_cohort_id,
            is_active,
            max(currentest.reporting_asof_date) filter (where is_active is true) over(partition by currentest.rsf_pfcbl_id) as last_active_date
          from (
            select distinct on (rd.rsf_pfcbl_id,rd.reporting_asof_date)
            rd.rsf_pfcbl_id,
            rd.reporting_asof_date,
            rd.reporting_cohort_id,
            case when rd.data_value = 'FALSE' then false else true end as is_active
            from activity_updates aup
            inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = aup.rsf_pfcbl_id
                                        and rd.indicator_id = aup.indicator_id
            order by 
              rd.rsf_pfcbl_id,
              rd.reporting_asof_date,
              rd.data_id desc
          ) as currentest 
        ) as closed 
        where (closed.reporting_asof_date > closed.last_active_date and is_active is false)
           or (closed.last_active_date is NULL and is_active is false) -- never active or closed-on entry
        order by 
          closed.rsf_pfcbl_id,
          closed.reporting_asof_date asc
      ) as deactivated on deactivated.rsf_pfcbl_id = updates.rsf_pfcbl_id
      where true
        and (
          (ids.deactivated_in_reporting_asof_date is distinct from deactivated.reporting_asof_date)
           or
          (ids.deactivated_by_reporting_cohort_id is distinct from deactivated.reporting_cohort_id)
        )       
    )
    update p_rsf.rsf_pfcbl_ids ids
    set deactivated_in_reporting_asof_date = sc.deactivated_in_reporting_asof_date,
        deactivated_by_reporting_cohort_id = sc.deactivated_by_reporting_cohort_id
    from status_change sc
    where sc.rsf_pfcbl_id = ids.rsf_pfcbl_id;   
          
    raise info ' - rsf_data_modified_data_current: (ACTIVE) completed update of rsf_pfcbl_ids in %',
    (select clock_timestamp()-msg_time); 
    
    
  end if;
  
  -----------------------------------------------------------------------------------------------
  -- DATA:5                 
  -- Finally! Let's insert our currentest data!
  msg_time := clock_timestamp();         
  
  with currentest as (
    select ins.data_id,ins.rsf_pfcbl_id,ins.indicator_id,ins.reporting_asof_date,ins.data_value,ins.data_unit,ins.reporting_cohort_id  
    from (
      select distinct on (mrd.rsf_pfcbl_id,mrd.indicator_id,mrd.reporting_asof_date)
        mrd.data_id,
        mrd.rsf_pfcbl_id,
        mrd.indicator_id,
        mrd.reporting_asof_date,
        mrd.data_value,
        mrd.data_unit,
        mrd.reporting_cohort_id  
      from modified_rsf_data mrd
      -- presumably all newly inserted data is newest, by definition.
      -- it shouldn't be possible to insert multiple reporting cohorts at a time.  But juuust in case something somewhere manages to do so,
      -- using distinct on, but only for modified_rsf_data 
      where TG_OP = 'INSERT' 
        and mrd.data_sys_flags&2 is distinct from 2
      order by mrd.rsf_pfcbl_id,mrd.indicator_id,mrd.reporting_asof_date,mrd.data_id desc -- most current data_id in timeline.
    ) ins
    
    UNION ALL
  
    select moddel.data_id,moddel.rsf_pfcbl_id,moddel.indicator_id,moddel.reporting_asof_date,moddel.data_value,moddel.data_unit,moddel.reporting_cohort_id  
    from (
      -- deletes can include numerous reporting cohorts at a time, so if a reporting timeline is deleted out of its appropriate timeline, get the latest value regardless (if any!)
      select distinct on (rd.rsf_pfcbl_id,rd.indicator_id,rd.reporting_asof_date)
        rd.data_id,
        rd.rsf_pfcbl_id,
        rd.indicator_id,
        rd.reporting_asof_date,
        rd.data_value,
        rd.data_unit,
        rd.reporting_cohort_id  
      from p_rsf.rsf_data rd
      inner join modified_rsf_data mrd on mrd.rsf_pfcbl_id = rd.rsf_pfcbl_id
                                      and mrd.indicator_id = rd.indicator_id
                                      and (mrd.reporting_asof_date = rd.reporting_asof_date
                                           or 
                                           mrd.data_id = rd.data_id) -- for updates, esp if sys flag changed timeline.
      where TG_OP <> 'INSERT' -- DELETE or UPDATE (updates can set flags that might result in soft-deletes or timeline update correction flags)
        and rd.data_sys_flags&2 is distinct from 2
      order by rd.rsf_pfcbl_id,rd.indicator_id,rd.reporting_asof_date,rd.data_id desc -- most current data_id in timeline.
    ) moddel
  ),
  current_data as (
    select
      cd.data_id,
      cd.rsf_pfcbl_id,
      cd.indicator_id,
      cd.reporting_asof_date,
      cd.data_value,
      cd.data_unit,
      --NULL::int as data_unit_data_id, 
      rc.reporting_time as data_time,
      rc.is_calculated_cohort is true as is_calculated_cohort,
      ind.is_periodic_or_flow_reporting,
      ind.data_type
    from currentest cd
    inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = cd.reporting_cohort_id
    inner join p_rsf.indicators ind on ind.indicator_id = cd.indicator_id  
  ),
  -- Currency data is special: ensure its data_unit value is not equal to LCU (and where it is, update that value) 
  data_currency as (
    select    
      cd.data_id,
			cd.rsf_pfcbl_id,
			cd.indicator_id,
			cd.reporting_asof_date,			
			cd.data_value,
            
			case when cd.data_unit = 'LCU'
			     then lcu.data_unit_value
					 else cd.data_unit
		  end as data_unit,
			
      -- data_unit_data_id means this unit is set equal to the LCU (whatever that may be) and if LCU changes this unit will get updated accordingly.
			case when cd.data_unit = 'LCU' or cd.data_unit = lcu.data_unit_value
					 then lcu.lcu_unit_data_id 
					 else NULL
			end as data_unit_data_id,
      cd.data_time,
      cd.is_calculated_cohort,
      cd.is_periodic_or_flow_reporting      
        
    from current_data cd
    left join lateral (select
                       lcu.data_unit_value,
                       lcu.lcu_unit_data_id
                     from p_rsf.rsf_data_current_lcu lcu -- all relevant current LCU data should have been update earlier in this trigger above
                     where lcu.for_rsf_pfcbl_id = cd.rsf_pfcbl_id
                       and lcu.reporting_asof_date <= cd.reporting_asof_date
                     order by lcu.reporting_asof_date desc
                     limit 1) as lcu on true
    where cd.data_type = 'currency'
  ),
  data_fxratio as (
    select 
    cd.data_id,
    cd.rsf_pfcbl_id,
    cd.indicator_id,
    cd.reporting_asof_date,

    -- ensure reported values are always in alphabetic order to reliably query values and know to multiply/divide results
    case when p_rsf.fx_currency_ratio_has_alphabetic_order(cd.data_unit) = false
         then (1/(cd.data_value::numeric))::text
       else cd.data_value
    end as data_value,

    case when p_rsf.fx_currency_ratio_has_alphabetic_order(cd.data_unit) = false
       then p_rsf.fx_currency_ratio_in_alphabetic_order(cd.data_unit)
       else cd.data_unit
    end as data_unit,
    
    NULL::int as data_unit_data_id,
    cd.data_time,
    cd.is_calculated_cohort,
    cd.is_periodic_or_flow_reporting
		from current_data cd
		left join lateral (select
												 lcu.data_unit_value,
												 lcu.lcu_unit_data_id
											 from p_rsf.rsf_data_current_lcu lcu 
											 where lcu.for_rsf_pfcbl_id = cd.rsf_pfcbl_id
											   and lcu.reporting_asof_date <= cd.reporting_asof_date
											 order by lcu.reporting_asof_date desc
											 limit 1) as lcu on true											 
    where cd.data_type = 'currency_ratio' -- currency_ratios only exist (currently) at
                                          -- facility level (values/sources defined by the facility) or
		                                      -- global level, sourced from IFC fx database  
  ),
  data_all as (
    
    select 
			dfx.data_id,
			dfx.rsf_pfcbl_id,
			dfx.indicator_id,
			dfx.reporting_asof_date,
			dfx.data_value,
			dfx.data_unit,
			dfx.data_unit_data_id,
      dfx.data_time,
      dfx.is_calculated_cohort,
      dfx.is_periodic_or_flow_reporting			
    from data_fxratio dfx

    UNION ALL
    
    select 
			dcu.data_id,
			dcu.rsf_pfcbl_id,
			dcu.indicator_id,
			dcu.reporting_asof_date,
			dcu.data_value,
			dcu.data_unit,
			dcu.data_unit_data_id,
      dcu.data_time,
      dcu.is_calculated_cohort,
      dcu.is_periodic_or_flow_reporting			
    from data_currency dcu
    
    UNION ALL
    
    select 
			cd.data_id,
			cd.rsf_pfcbl_id,
			cd.indicator_id,
			cd.reporting_asof_date,
			cd.data_value,
			cd.data_unit,
			NULL::int as data_unit_data_id,
      cd.data_time,
      cd.is_calculated_cohort,
      cd.is_periodic_or_flow_reporting			
		from current_data cd
		where cd.data_type not in ('currency','currency_ratio')  
  )
  insert into p_rsf.rsf_data_current(data_id,
                                     rsf_pfcbl_id,
                                     indicator_id,
                                     reporting_asof_date,
                                     data_value,
                                     data_unit,
                                     data_unit_data_id,
                                     data_time,
                                     is_calculated,
                                     is_periodic)
   select 
    dall.data_id,
    dall.rsf_pfcbl_id,
    dall.indicator_id,
    dall.reporting_asof_date,
    dall.data_value,
    dall.data_unit,
    dall.data_unit_data_id,
    dall.data_time,
    dall.is_calculated_cohort as is_calculated,
    dall.is_periodic_or_flow_reporting as is_periodic		
  from data_all dall
  on conflict(rsf_pfcbl_id,indicator_id,reporting_asof_date) -- rsf_pfcbl_id,indicator_id,reporting_asof_date
	do update
	set
		data_id = EXCLUDED.data_id,
		data_value = EXCLUDED.data_value,
		data_unit = EXCLUDED.data_unit,
		data_unit_data_id = EXCLUDED.data_unit_data_id,
    data_time = EXCLUDED.data_time,
    is_calculated = EXCLUDED.is_calculated,
    is_periodic = EXCLUDED.is_periodic;
    
	
  raise info ' - rsf_data_modified_data_current: (DATA) completed update of rsf_data_current in %',
  (select clock_timestamp()-msg_time); 

	return NULL;
		
END;
$BODY$
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
DROP FUNCTION IF EXISTS "p_rsf"."rsf_pfcbl_generate_reporting_dates"("v_rsf_pfcbl_id" int4, "v_until_date" date, "v_stop_at_deactivation_date" bool);
CREATE FUNCTION "p_rsf"."rsf_pfcbl_generate_reporting_dates"("v_rsf_pfcbl_id" int4, "v_until_date" date=(now())::date, "v_stop_at_deactivation_date" bool=false)
  RETURNS TABLE("rsf_pfcbl_id" int4, "valid_reporting_date" date, "reporting_sequence_rank" int4, "is_deactivated" bool) AS $BODY$
begin 							

  return query							
  SELECT 
    ids.rsf_pfcbl_id,
    (dates.valid_reporting_date - ('1 day'::interval))::date as valid_reporting_date,
    row_number() OVER (PARTITION BY ids.rsf_pfcbl_id 
                       ORDER BY (dates.valid_reporting_date - '1 day'::interval)::date)::int AS reporting_sequence_rank,
                       
    ids.deactivated_in_reporting_asof_date is not null
    or ids.deactivated_in_reporting_asof_date <=  (dates.valid_reporting_date - ('1 day'::interval))::date 
    
  FROM p_rsf.rsf_pfcbl_ids ids
  INNER JOIN LATERAL (select 
                      case when v_stop_at_deactivation_date is true
                           then least(ids.deactivated_in_reporting_asof_date::date,
                                      greatest(now()::date,v_until_date::date))::timestamp
                           else greatest(now()::date,v_until_date::date)::timestamp end as stop_date) as stops on true
                           
  INNER JOIN LATERAL (select * 
                      from generate_series(
                        date_trunc('quarter'::text,(ids.created_in_reporting_asof_date::date)::timestamp with time zone),         
                        stops.stop_date::timestamp with time zone + '3 mons'::interval,
                        '3 mons'::interval) as valid_reporting_date) dates on true
  where ids.rsf_pfcbl_id = v_rsf_pfcbl_id
  and dates.valid_reporting_date >= ids.created_in_reporting_asof_date::date;
  

end; 
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100
  ROWS 1000;

-- ----------------------------
-- Function structure for rsf_pfcbl_id_deleted_archive
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_pfcbl_id_deleted_archive"();
CREATE FUNCTION "p_rsf"."rsf_pfcbl_id_deleted_archive"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
--DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

  if (OLD.pfcbl_category_rank <= 2) -- global,program,facility
  then  
  
  insert into p_rsf.rsf_setup_archive(sys_name,
                                     reporting_asof_date,
                                     pfcbl_name,
                                     pfcbl_category,
                                     settings_source,
                                     settings_value,
                                     is_disabled,
                                     is_restored,
                                     archive_time,
                                     restored_by_reporting_cohort_id)
  select 
    rsa.sys_name,
    rsa.reporting_asof_date,
    rsa.pfcbl_name,
    OLD.pfcbl_category,
    rsa.settings_source,
    rsa.settings_value,
    false,
    false,
    now()::timestamptz as archive_time,
    NULL as restored_by_reporting_cohort_id
  from p_rsf.view_rsf_setup_archive rsa 
  where rsa.rsf_pfcbl_id = OLD.rsf_pfcbl_id  
  on conflict(sys_name,settings_source)
  do update
  set reporting_asof_date = EXCLUDED.reporting_asof_date,
      settings_value = EXCLUDED.settings_value,
      is_disabled = EXCLUDED.is_disabled,
      is_restored = EXCLUDED.is_restored,
      archive_time = EXCLUDED.archive_time,
      restored_by_reporting_cohort_id = EXCLUDED.restored_by_reporting_cohort_id;  
  
  --raise notice 'rsf_pfcbl_id_deleted_archive %',(clock_timestamp()-msg_time);         
  end if;   
        

  
  return OLD;
  
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_pfcbl_id_insert_family_ids
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_pfcbl_id_insert_family_ids"();
CREATE FUNCTION "p_rsf"."rsf_pfcbl_id_insert_family_ids"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN
/*
update p_rsf.rsf_pfcbl_ids ids
set rsf_pf_id = coalesce(rsf_facility_id,rsf_program_id),
  rsf_gpfcbl_family = 
    array_remove(
      array[0,
            rsf_program_id,
            rsf_facility_id,
            rsf_client_id,
            rsf_borrower_id,
            rsf_loan_id],
      NULL);
*/

  NEW.rsf_pf_id := coalesce(NEW.rsf_facility_id,NEW.rsf_program_id);
  NEW.rsf_gpfcbl_family := 
    uniq(
      array_remove(
        array[NULLIF(NEW.rsf_pfcbl_id,0),
              NEW.rsf_program_id,
              NEW.rsf_facility_id,
              NEW.rsf_client_id,
              NEW.rsf_borrower_id,
              NEW.rsf_loan_id],
        NULL));
  return NEW;
  
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for rsf_pfcbl_indicator_recalculate
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_pfcbl_indicator_recalculate"("v_rsf_pfcbl_id" int4, "v_formula_id" int4);
CREATE FUNCTION "p_rsf"."rsf_pfcbl_indicator_recalculate"("v_rsf_pfcbl_id" int4, "v_formula_id" int4)
  RETURNS "pg_catalog"."bool" AS $BODY$
  DECLARE msg_time timestamp not null default clock_timestamp();
BEGIN

raise exception 'DEPRECATED! rsf_pfcbl_indicator_recalculate() for rsf_pfcbl_id=% formula_id=% and trigger_depth=% -- use p_rsf.view_rsf_pf_calculation_evaluations_required',
v_rsf_pfcbl_id,
	v_formula_id,
	pg_trigger_depth();
  
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
  
return true;
END;
$BODY$
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

  with remove_obsolete as (
    select 
      ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id,
      ch.check_formula_id    
    from changed as ch
    inner join p_rsf.indicator_check_formulas icf on icf.check_formula_id = ch.check_formula_id
    inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = ch.rsf_pfcbl_id
                                                     and ft.to_pfcbl_category = icf.check_pfcbl_category
    where ch.is_subscribed is false
  
  )
  delete from p_rsf.rsf_data_checks rdc
  using remove_obsolete ro
  where ro.rsf_pfcbl_id = rdc.rsf_pfcbl_id
    and ro.check_formula_id = rdc.check_formula_id;
    
-- disabled this auto-subscribe feature because subscribing to the check had a cascading effect on the indicators and with new indicators being subscribed,
-- new checks being subscribed, etc... perhaps unintentionally and spamming the setup a bit.
-- better approach: apply error flags for checks whose parameters are not monitored
return null;
/*
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
*/  
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
      where (scm.is_auto_monitorable is true or scm.is_contract_monitorable is true)
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
  
  update p_rsf.rsf_setup_indicators rsi
  set 
      formula_id = changed.formula_id,
      is_subscribed = changed.is_subscribed, -- all indicators I managed must be equally subscribed
      is_auto_subscribed = changed.is_auto_subscribed,
      comments_user_id = changed.comments_user_id,
      auto_subscribed_by_reporting_cohort_id = changed.auto_subscribed_by_reporting_cohort_id
   from changed 
   inner join p_rsf.indicators ind on ind.unit_fx_indicator_id = changed.indicator_id
   where rsi.rsf_pfcbl_id = changed.rsf_pfcbl_id
     and rsi.indicator_id = ind.indicator_id; 



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
      -- formula_id may not be unique due to unit_fx indicators having the SAME formula
      -- so join on the formula AND the indicator that defines that formula
      inner join p_rsf.indicator_formula_parameters ifp on ifp.formula_id = changed.formula_id
                                                       and ifp.indicator_id = changed.indicator_id 
      inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = changed.rsf_pfcbl_id
                                                                 and sis.indicator_id = ifp.parameter_indicator_id
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = sis.rsf_pfcbl_id
      where sis.category_manager_rsf_pfcbl_id is not null
      on conflict --(rsf_pfcbl_id,indicator_id)
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


      raise exception 'Only Global, Program and Facility may setup indicator subscriptions: Global for Global Indicators; Program for Program Indicators; Facility for Facility+ Indicators';
      return null;
      /*
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
        */
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
  
  
    with evals as materialized (
      select distinct
        changed.rsf_pfcbl_id as rsf_pf_id,
        changed.formula_id,
        changed.indicator_id as calculate_indicator_id
      from changed
      where changed.is_subscribed is true
        and (changed.formula_id is not null or exists(select true from p_rsf.indicators ind 
                                                      where ind.indicator_id = changed.indicator_id
                                                        and ind.unit_fx_indicator_id is not null))
                                                          
    )
    insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date,rsf_pf_id,formula_calculation_rank)
    select distinct
      calc.calculate_rsf_pfcbl_id,
      calc.calculate_indicator_id,
      calc.calculate_asof_date,
      calc.to_rsf_pf_id,
      calc.to_formula_calculation_rank
    from evals
    cross join lateral (
      select
        cer.calculate_rsf_pfcbl_id,
        cer.calculate_indicator_id,
        cer.calculate_asof_date,
        cer.to_rsf_pf_id,
        cer.to_formula_calculation_rank
      from p_rsf.view_rsf_pf_calculation_evaluations_required cer
      where cer.from_rsf_pf_id = evals.rsf_pf_id        
        and cer.calculate_indicator_id = evals.calculate_indicator_id
      offset 0 -- offset 0 is a "hack" for the query planner to ensure the cohort is fully collapsed in-line; which it doesn't do, even with "materialized"
    ) as calc;


    -- We don't want to recalculate etc if they just make a setup comment, etc.
  elseif TG_OP = 'UPDATE' then


    with evals as materialized (
      select distinct
        changed.rsf_pfcbl_id as rsf_pf_id,
        changed.indicator_id as calcuate_indicator_id,
        changed.formula_id,
        changed.formula_calculation_unit
        from changed
        where changed.is_subscribed is true
          and (changed.formula_id is not null or exists(select true from p_rsf.indicators ind 
                                                        where ind.indicator_id = changed.indicator_id
                                                          and ind.unit_fx_indicator_id is not null))
        
        except
        
        select 
        previous.rsf_pfcbl_id as rsf_pf_id,
        previous.indicator_id as calcuate_indicator_id,
        previous.formula_id,
        previous.formula_calculation_unit
        from previous     
    )
    insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date,rsf_pf_id,formula_calculation_rank)
    select distinct
      calc.calculate_rsf_pfcbl_id,
      calc.calculate_indicator_id,
      calc.calculate_asof_date,
      calc.to_rsf_pf_id,
      calc.to_formula_calculation_rank
    from evals
    cross join lateral (
      select
        cer.calculate_rsf_pfcbl_id,
        cer.calculate_indicator_id,
        cer.calculate_asof_date,
        cer.to_rsf_pf_id,
        cer.to_formula_calculation_rank
      from p_rsf.view_rsf_pf_calculation_evaluations_required cer
      where cer.from_rsf_pf_id = evals.rsf_pf_id        
        and cer.calculate_indicator_id = evals.calculate_indicator_id
      offset 0 -- offset 0 is a "hack" for the query planner to ensure the cohort is fully collapsed in-line; which it doesn't do, even with "materialized"
    ) as calc;


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
  
  --If my unit is managed by another indicator... 
  if (exists(select true from p_rsf.indicators ind 
             where ind.indicator_id = NEW.indicator_id
               and ind.unit_fx_indicator_id is not null))
  then
  
    -- ...then that other indicator needs to be subscribed, too!
    if (not exists(select true 
                   from p_rsf.rsf_setup_indicators rsi
                   where rsi.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                     and rsi.indicator_id = (select ind.unit_fx_indicator_id
                                             from p_rsf.indicators ind
                                             where ind.indicator_id = NEW.indicator_id)))
    then  
        
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
       select 
       NEW.rsf_pfcbl_id,
       ind.indicator_id,
       indf.formula_id,
       ids.rsf_program_id,
       ids.rsf_facility_id,
       NEW.is_subscribed,
       NEW.is_auto_subscribed,
       concat('SYSTEM: auto-subscription triggered by ',NEW.indicator_id,'.','\n' || NEW.subscription_comments),
       NEW.comments_user_id,
       NEW.options_group_id,
       NULL as formula_calculation_unit,
       NEW.auto_subscribed_by_reporting_cohort_id
       from p_rsf.rsf_pfcbl_ids ids 
       inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
       left join p_rsf.indicator_formulas indf on indf.indicator_id = ind.indicator_id
                                              and indf.is_primary_default is true
       where ids.rsf_pfcbl_id = NEW.rsf_pfcbl_id
         and ind.unit_fx_indicator_id = NEW.indicator_id;
        
    
    end if;                                               
    
    NEW.formula_calculation_unit := NULL; -- can only have  unit_fx_indicator_id for currencies taht are not LCU and are defined (and cannot be overwritten)
    NEW.formula_id := (select rsi.formula_id
                       from p_rsf.rsf_setup_indicators rsi
                       where rsi.rsf_pfcbl_id = NEW.rsf_pfcbl_id
                         and rsi.indicator_id = (select ind.unit_fx_indicator_id
                                                 from p_rsf.indicators ind
                                                 where ind.indicator_id = NEW.indicator_id));
  end if;                 
               
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
-- Function structure for rsf_setup_template_headers_normalized
-- ----------------------------
DROP FUNCTION IF EXISTS "p_rsf"."rsf_setup_template_headers_normalized"();
CREATE FUNCTION "p_rsf"."rsf_setup_template_headers_normalized"()
  RETURNS "pg_catalog"."trigger" AS $BODY$
BEGIN

  NEW.template_header_full_normalized := coalesce(trim(concat(normalizeLabel(NEW.template_header_sheet_name),normalizeLabel(NEW.template_header))),'');
  NEW.action_mapping := coalesce(NEW.action,'default') || greatest(NEW.map_indicator_id,NEW.map_formula_id,NEW.map_check_formula_id,0);

  return NEW;
END; $BODY$
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
																									 parameter_data_type,
                                                   calculate_pfcbl_rank)
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
			ind_p.data_type as parameter_data_type,
      ind.pfcbl_rank

		from p_rsf.indicators ind
		inner join p_rsf.rsf_pfcbl_categories rpc_f on rpc_f.pfcbl_category = ind.data_category
		inner join lateral (select distinct unnest(NEW.formula_indicator_ids) as parameter_indicator_id) pid on true -- should alrady be distinct
		inner join p_rsf.indicators ind_p on ind_p.indicator_id = pid.parameter_indicator_id
		inner join p_rsf.rsf_pfcbl_categories rpc_p on rpc_p.pfcbl_category = ind_p.data_category
		left join p_rsf.rsf_pfcbl_categories rpc_g on rpc_g.pfcbl_rank = NEW.formula_grouping_pfcbl_rank
		                                          and rpc_g.pfcbl_rank < rpc_f.pfcbl_rank -- it's only meaningful to group at a parent level
		where ind.indicator_id = NEW.indicator_id;
		 
     
    insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date,rsf_pf_id,formula_calculation_rank)
    select distinct
      calc.calculate_rsf_pfcbl_id,
      calc.calculate_indicator_id,
      calc.calculate_asof_date,
      calc.to_rsf_pf_id,
      calc.to_formula_calculation_rank
    from p_rsf.view_rsf_pf_calculation_evaluations_required calc 
    where calc.calculate_indicator_id = NEW.indicator_id
    on conflict do nothing;
    
    ------------------------------------------------------------------------------------------------
		-- deprecated Jun-2026 -
    -- refresh materialized view p_rsf.compute_calculation_to_parameter_categories;
		------------------------------------------------------------------------------------------------
    
   /*
  perform * from (
    select pfi.rsf_pfcbl_id,pfi.formula_id,recalc
    from p_rsf.rsf_setup_indicators pfi
    inner join lateral p_rsf.rsf_pfcbl_indicator_recalculate(v_rsf_pfcbl_id => pfi.rsf_pfcbl_id,
                                                             v_formula_id => pfi.formula_id) as recalc on true
    where pfi.indicator_id = NEW.indicator_id
      and pfi.formula_id is not null
      and pfi.is_subscribed is true);
*/
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

    NEW.computation_group :=
      case when NEW.parent_grouping_pfcbl_category is NULL then 1
           when NEW.parent_grouping_pfcbl_category is NOT NULL and 5 = any(NEW.parameter_pfcbl_ranks) then 2
           when NEW.parent_grouping_pfcbl_category is NOT NULL and 4 = any(NEW.parameter_pfcbl_ranks)  then 3
           when NEW.parent_grouping_pfcbl_category is NOT NULL and 3 = any(NEW.parameter_pfcbl_ranks)   then 4
           when NEW.parent_grouping_pfcbl_category is NOT NULL and 2 = any(NEW.parameter_pfcbl_ranks)  then 4
           else 6 end;
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

	
	                                                     
  update p_rsf.indicator_formulas indf
  set formula_calculation_rank = ccr.calculation_rank,
      formula_indicator_id_requirements = ccr.formula_indicator_id_requirements
      --computation_priority_rank = case when ccr.data_type = 'currency_ratio' then 1 else 0 end
  from p_rsf.compute_calculation_ranks ccr
  where ccr.formula_id = indf.formula_id
    and (ccr.calculation_rank is distinct from indf.formula_calculation_rank
         or
         ccr.formula_indicator_id_requirements is distinct from indf.formula_indicator_id_requirements);
         
         -- removed computation_priority_rank as its only used in this one instances and only for sorting stale calculations, so just removed
         -- column and sorted on currency_ratio directly.
         --or
         --indf.computation_priority_rank is distinct from case when ccr.data_type = 'currency_ratio' then 1 else 0 end);
		
 
 
      --NEW.formula_indicator_id_requirements := uniq(sort(NEW.formula_indicator_id_requirements));
			--raise notice '    Set formula_calculation_rank for indicator_id=% as % and requirements as %',
			--NEW.indicator_id,NEW.formula_calculation_rank,array_to_string(NEW.formula_indicator_id_requirements,',');

      if exists(select * from p_rsf.indicator_formulas indf
                inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
                where ind.data_type = 'currency_ratio'
                  and ind.data_category <> 'global'
                  and indf.formula_calculation_rank <> 0)
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
			
      if (exists(select true from p_rsf.indicators ind
                 where ind.indicator_id = NEW.indicator_id
                   and ind.unit_fx_indicator_id is not null))
      then
        raise exception 'A formula may not be defined for this indicator as it is already defined as a fx-unit managed by another indicator';
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


      NEW.computation_group := 
        case when NEW.formula_grouping_pfcbl_rank is NULL then 1 -- only self and parents, can group together
             when NEW.formula_grouping_pfcbl_rank is NOT NULL and 5 = any(NEW.formula_pfcbl_rank_range) then 2 -- from any parent to loan-level
             when NEW.formula_grouping_pfcbl_rank is NOT NULL and 4 = any(NEW.formula_pfcbl_rank_range) then 3 -- from any parent to borrower-level
             when NEW.formula_grouping_pfcbl_rank is NOT NULL and 3 = any(NEW.formula_pfcbl_rank_range)  then  4 -- from any parent to loan-level: same
             when NEW.formula_grouping_pfcbl_rank is NOT NULL and 2 = any(NEW.formula_pfcbl_rank_range)  then 4 -- from any parent to loan-level: same
             else 6 end;

    NEW.has_reporting_parameters := 
      exists(select true
             from p_rsf.indicators ind
             where ind.indicator_sys_category = 'entity_reporting'
               and ind.indicator_id = any(NEW.formula_indicator_ids));
    
    NEW.has_timeseries_parameters := coalesce(NEW.formula ~ '\.all|\.previous',false);
    
    NEW.has_no_parameters := coalesce(cardinality(NEW.formula_indicator_ids),0) = 0;
    
/* Jan 2024: Rewrote rankings to Exclude global parameters from ranking because FX calculations were getting rank of 2 when
   their only paramers are global date updates.  And all global calculations are independently calculated of rank, first.
	 This is because some rank 1 calculations were requesting fx rates that had not yet been validated due to their rank being 2
*/
/* Jul 2025: Global parameters are no longer excluded.  It doesn't gain any efficiency and it adds complexity */

   if NEW.formula_calculation_rank is NULL 
      OR
      NEW.formula_indicator_id_requirements is NULL
   then 
     NEW.formula_calculation_rank := -1;
     NEW.formula_indicator_id_requirements := array[]::int[];
   end if;


   if exists(select * from p_rsf.indicators ind 
                where ind.indicator_id = NEW.indicator_id
                  and ind.data_type = 'currency_ratio'
                  and 1 <= any(NEW.formula_pfcbl_rank_range))
      then
        raise exception 'Failed to update formula: currency_ratio indicator formulas can only have global variables used in their formulas. Formulas are generally expected to take the form of: get_IFC_FX_rate(exchange_rate_date=global_reporting_quarter_end_date.current,currency_code_ratio="UAH/USD")';
      end if;
		

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
-- View structure for view_indicator_checks_data_is_correctable
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_indicator_checks_data_is_correctable";
CREATE VIEW "p_rsf"."view_indicator_checks_data_is_correctable" AS  SELECT ic.indicator_check_id,
    ic.check_name,
    ic.check_type,
    ic.check_class,
    icf.check_formula_id,
    icf.check_formula_title,
    ind.indicator_name AS correctable_indicator_name,
    ind.indicator_id AS correctable_indicator_id,
    dsf.data_flag_name,
    dsf.data_flag_value
   FROM p_rsf.indicator_check_formulas icf
     JOIN p_rsf.rsf_data_sys_flags dsf ON dsf.data_flag_name = 'CORRECTION'::text
     JOIN LATERAL ( SELECT (regexp_match((regexp_matches(icf.formula, '\y([a-zA-Z_]+\.current\.changed)[\s=,]+TRUE|\y([a-zA-Z_.]+\.current\.updated)[\s=,]+TRUE'::text, 'gi'::text))[1], '^([a-z_]+)\..*$'::text, 'i'::text))[1] AS indicator_name) cu_ind ON true
     JOIN p_rsf.indicators ind ON ind.indicator_name::text = cu_ind.indicator_name AND ind.data_category::text = icf.check_pfcbl_category
     JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = icf.indicator_check_id
  WHERE ic.check_type = 'business_integrity'::text AND ind.is_system IS FALSE
UNION ALL
 SELECT ic.indicator_check_id,
    ic.check_name,
    ic.check_type,
    ic.check_class,
    NULL::integer AS check_formula_id,
    'System check'::text AS check_formula_title,
    ind.indicator_name AS correctable_indicator_name,
    ind.indicator_id AS correctable_indicator_id,
    dsf.data_flag_name,
    dsf.data_flag_value
   FROM p_rsf.indicator_checks ic
     JOIN p_rsf.rsf_data_sys_flags dsf ON dsf.data_flag_name = 'CORRECTION'::text
     JOIN p_rsf.indicators ind ON ic.check_name::text ~ ind.data_category::text
  WHERE ic.is_system IS TRUE AND (ic.data_sys_flags_granted & dsf.data_flag_value::integer) = 16 AND NOT (EXISTS ( SELECT true
           FROM p_rsf.indicator_check_formulas icf
          WHERE icf.indicator_check_id = ic.indicator_check_id)) AND ind.is_system IS FALSE;

-- ----------------------------
-- View structure for util_reporting_cohort_info_log_times
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."util_reporting_cohort_info_log_times";
CREATE VIEW "p_rsf"."util_reporting_cohort_info_log_times" AS  SELECT reporting_imports.import_id,
    reporting_imports.file_name,
    stimes.st AS statement_time,
    ttimes.tt AS total_time,
    "substring"(log.log, '([A-Za-z_]+)'::text) AS func,
    log.log
   FROM p_rsf.reporting_imports
     LEFT JOIN LATERAL unnest(string_to_array(reporting_imports.metadata ->> 'log'::text, '\n'::text)) log(log) ON true
     LEFT JOIN LATERAL ( SELECT (regexp_match(log.log, 'Done!\s?@?(\d+\.?\d*).+'::text))[1]::numeric AS st) stimes ON true
     LEFT JOIN LATERAL ( SELECT (regexp_match(log.log, 'running total @(\d+\.?\d*)'::text))[1]::numeric AS tt) ttimes ON true
  WHERE log.log IS NOT NULL;

-- ----------------------------
-- View structure for view_rsf_setup_check_config
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_check_config";
CREATE VIEW "p_rsf"."view_rsf_setup_check_config" AS  SELECT ids.rsf_pfcbl_id,
    ids.pfcbl_category,
    scc.for_indicator_id,
    ic.indicator_check_id,
    scc.check_formula_id,
    ids.rsf_program_id,
    ids.rsf_facility_id,
    true AS is_subscribed,
    false AS is_unsubscribed,
    false AS is_auto_subscribed,
    COALESCE(scc.config_auto_resolve, ic.auto_resolve_system_check, false) AS config_auto_resolve,
    COALESCE(scc.config_check_class, ic.check_class::text) AS config_check_class,
        CASE
            WHEN ic.variance_tolerance_allowed IS FALSE THEN NULL::numeric
            ELSE COALESCE(scc.config_threshold, 0::numeric) *
            CASE
                WHEN ind.data_type::text <> 'date'::text THEN 100
                ELSE 1
            END::numeric
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
        CASE
            WHEN data_type::text = 'currency_ratio'::text AND data_category::text <> 'global'::text THEN max(crank) - 1
            ELSE max(crank)
        END AS calculation_rank,
    array_remove(array_agg(DISTINCT parameter_id ORDER BY parameter_id), NULL::integer) AS formula_indicator_id_requirements,
    sum(here::integer) AS nested_ranks
   FROM ranks
  GROUP BY formula_id, data_category, data_type;

-- ----------------------------
-- View structure for util_reporting_cohort_info_process_times
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."util_reporting_cohort_info_process_times";
CREATE VIEW "p_rsf"."util_reporting_cohort_info_process_times" AS  SELECT sn.sys_name,
    rci.reporting_asof_date,
    rci.import_rsf_pfcbl_id,
    rci.import_id,
    ((rci.metadata -> 'timing'::text) ->> 'parse_time'::text)::numeric AS parse_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'process_time'::text)::numeric AS process_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'upload_time'::text)::numeric AS upload_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'total_time'::text)::numeric AS total_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'backup_time'::text)::numeric AS backup_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'calculate_time'::text)::numeric AS calculate_time_sec,
    ((rci.metadata -> 'timing'::text) ->> 'check_time'::text)::numeric AS check_time_sec
   FROM p_rsf.reporting_imports rci
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = rci.import_rsf_pfcbl_id;

-- ----------------------------
-- View structure for util_reporting_cohort_times_by_entity
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."util_reporting_cohort_times_by_entity";
CREATE VIEW "p_rsf"."util_reporting_cohort_times_by_entity" AS  SELECT sys_name,
    import_rsf_pfcbl_id,
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
  GROUP BY sys_name, import_rsf_pfcbl_id
  ORDER BY sys_name;

-- ----------------------------
-- View structure for view_rsf_pf_calculation_parameter_requirements
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pf_calculation_parameter_requirements";
CREATE VIEW "p_rsf"."view_rsf_pf_calculation_parameter_requirements" AS  SELECT DISTINCT rsi.rsf_pfcbl_id AS from_rsf_pf_id,
    f.parameter_pf_id AS to_parameter_pf_id,
    indf.formula_id,
    ind.indicator_id,
    ind.pfcbl_rank AS data_category_rank,
    indf.formula_calculation_rank,
    ind.unit_fx_indicator_id,
    ind.unit_fx_method
   FROM ( SELECT gpf_rsi.rsf_pfcbl_id,
            gpf_rsi.formula_id,
            gpf_rsi.indicator_id
           FROM p_rsf.rsf_setup_indicators gpf_rsi
             JOIN p_rsf.indicators gpf_ind ON gpf_ind.indicator_id = gpf_rsi.indicator_id
          WHERE gpf_rsi.is_subscribed IS TRUE AND (gpf_rsi.formula_id IS NOT NULL OR gpf_ind.unit_fx_indicator_id IS NOT NULL)
        UNION ALL
         SELECT fp_ids.rsf_facility_id AS rsf_pfcbl_id,
            fp_rsi.formula_id,
            fp_rsi.indicator_id
           FROM p_rsf.rsf_setup_indicators fp_rsi
             JOIN p_rsf.rsf_pfcbl_ids fp_ids ON fp_ids.rsf_program_id = fp_rsi.rsf_pfcbl_id
          WHERE fp_ids.pfcbl_category_rank = 2 AND ((EXISTS ( SELECT true
                   FROM p_rsf.indicator_formula_parameters ifp
                  WHERE ifp.formula_id = fp_rsi.formula_id AND ifp.parameter_pfcbl_rank > 1)) OR (EXISTS ( SELECT true
                   FROM p_rsf.indicators indfp
                  WHERE indfp.indicator_id = fp_rsi.indicator_id AND indfp.unit_fx_indicator_id IS NOT NULL AND indfp.pfcbl_rank > 1))) AND fp_rsi.is_subscribed IS TRUE) rsi
     JOIN p_rsf.rsf_pfcbl_ids ids ON ids.rsf_pfcbl_id = rsi.rsf_pfcbl_id
     JOIN p_rsf.indicators ind ON ind.indicator_id = rsi.indicator_id
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.formula_id = rsi.formula_id
     CROSS JOIN LATERAL ( SELECT ids.rsf_pfcbl_id
          WHERE indf.formula_pfcbl_rank_range IS NULL
        UNION ALL
         SELECT 0 AS rsf_pfcbl_id
          WHERE 0 = ANY (indf.formula_pfcbl_rank_range)
        UNION ALL
         SELECT ids.rsf_program_id AS rsf_pfcbl_id
          WHERE 1 = ANY (indf.formula_pfcbl_rank_range)
        UNION ALL
         SELECT ids.rsf_facility_id AS rsf_pfcbl_id
          WHERE ids.pfcbl_category_rank = 2 AND ARRAY[2, 3, 4, 5] && indf.formula_pfcbl_rank_range::integer[]
        UNION ALL
         SELECT cids.rsf_facility_id AS rsf_pfcbl_id
           FROM p_rsf.rsf_pfcbl_ids cids
          WHERE cids.rsf_program_id = ids.rsf_program_id AND cids.pfcbl_category_rank = 2 AND ids.pfcbl_category_rank = 1 AND ARRAY[2, 3, 4, 5] && indf.formula_pfcbl_rank_range::integer[]) f(parameter_pf_id);

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
    nai.pfcbl_name,
    concat(COALESCE(nai.nickname, nai.name, 'RANK'::text || nai.rank_id, ('MISSING '::text || upper(ids.pfcbl_category::text)) || ' NAME'::text), ' (', COALESCE(nai.id, 'SYSID'::text || ids.rsf_pfcbl_id), ')') AS rsf_full_name,
    COALESCE(nai.nickname, nai.name, 'RANK'::text || nai.rank_id, ('MISSING '::text || upper(ids.pfcbl_category::text)) || ' NAME'::text) AS rsf_name
   FROM p_rsf.rsf_pfcbl_ids ids
     LEFT JOIN p_rsf.rsf_data_current_names_and_ids nai ON nai.rsf_pfcbl_id = ids.rsf_pfcbl_id
  ORDER BY ids.rsf_pfcbl_id, nai.reporting_asof_date DESC NULLS LAST;

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
-- View structure for view_account_info
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_account_info";
CREATE VIEW "p_rsf"."view_account_info" AS  SELECT account_id,
    users_name,
    login_email AS users_login,
    is_system_account
   FROM p_rsf.dblink_account_info() dblink_account_info(account_id, users_name, login_email, is_system_account);

-- ----------------------------
-- View structure for view_reporting_imports_data_checks_current_active
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_reporting_imports_data_checks_current_active";
CREATE VIEW "p_rsf"."view_reporting_imports_data_checks_current_active" AS  SELECT ri.import_id,
    ri.import_rsf_pfcbl_id,
    chk.check_asof_date,
    count(*) AS data_checks_active,
    count(*) FILTER (WHERE COALESCE(scc.config_check_class, ic.check_class::text) = 'critical'::text) AS data_checks_critical_active,
    count(*) FILTER (WHERE COALESCE(scc.config_check_class, ic.check_class::text) = 'error'::text) AS data_checks_error_active,
    count(*) FILTER (WHERE COALESCE(scc.config_check_class, ic.check_class::text) = 'warning'::text) AS data_checks_warning_active,
    count(*) FILTER (WHERE COALESCE(scc.config_check_class, ic.check_class::text) = 'info'::text) AS data_checks_info_active,
    count(*) FILTER (WHERE chk.check_status_comment IS NULL) AS data_checks_new,
    count(*) FILTER (WHERE COALESCE(scc.config_check_class, ic.check_class::text) = 'critical'::text AND chk.check_status_comment IS NULL) AS data_checks_critical_new,
    count(*) FILTER (WHERE COALESCE(scc.config_check_class, ic.check_class::text) = 'error'::text AND chk.check_status_comment IS NULL) AS data_checks_error_new,
    count(*) FILTER (WHERE COALESCE(scc.config_check_class, ic.check_class::text) = 'warning'::text AND chk.check_status_comment IS NULL) AS data_checks_warning_new,
    count(*) FILTER (WHERE COALESCE(scc.config_check_class, ic.check_class::text) = 'info'::text AND chk.check_status_comment IS NULL) AS data_checks_info_new
   FROM p_rsf.reporting_imports ri
     JOIN p_rsf.reporting_cohorts rc ON rc.import_id = ri.import_id
     JOIN p_rsf.rsf_data rd ON rd.reporting_cohort_id = rc.reporting_cohort_id
     JOIN p_rsf.rsf_data_checks chk ON chk.data_id = rd.data_id AND chk.check_status = 'active'::text
     JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = chk.indicator_check_id
     LEFT JOIN p_rsf.view_rsf_setup_check_config scc ON scc.rsf_pfcbl_id = ri.import_rsf_pfcbl_id AND scc.for_indicator_id = rd.indicator_id AND scc.indicator_check_id = chk.indicator_check_id AND NOT scc.check_formula_id IS DISTINCT FROM chk.check_formula_id
  WHERE chk.check_data_id_is_current IS TRUE
  GROUP BY ri.import_id, ri.import_rsf_pfcbl_id, chk.check_asof_date;

-- ----------------------------
-- View structure for view_rsf_setup_export_reporting
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_export_reporting";
CREATE VIEW "p_rsf"."view_rsf_setup_export_reporting" AS  SELECT COALESCE(pnids.nickname, pnids.name) AS program_name,
    COALESCE(ids.rsf_facility_id, ids.rsf_program_id) AS entity_id,
    COALESCE(nids.id, ids.rsf_pfcbl_id::text) AS id,
    nids.name,
    nids.rsf_full_name,
    ri.import_id,
    ri.file_name,
    ri.file_data,
    ri.import_user_id,
    ri.import_time,
    ri.import_comments,
    ri.reporting_asof_date,
    ids.created_in_reporting_asof_date,
    rt.template_id,
    rt.template_name,
    EXTRACT(year FROM ri.reporting_asof_date) * 4::numeric + EXTRACT(quarter FROM ri.reporting_asof_date) - (EXTRACT(year FROM ids.created_in_reporting_asof_date) * 4::numeric + EXTRACT(quarter FROM ids.created_in_reporting_asof_date)) AS quarter_diff,
    COALESCE(' '::text || NULLIF(dense_rank() OVER (PARTITION BY nids.rsf_pfcbl_id, ri.reporting_asof_date ORDER BY ri.import_id), 1)::text, ''::text) AS seq_num,
    sn.sys_name
   FROM p_rsf.rsf_pfcbl_ids ids
     JOIN p_rsf.reporting_imports ri ON ri.import_rsf_pfcbl_id = ids.rsf_pfcbl_id
     JOIN p_rsf.reporting_templates rt ON rt.template_id = ri.template_id
     JOIN p_rsf.view_current_entity_names_and_ids nids ON nids.rsf_pfcbl_id = COALESCE(ids.rsf_facility_id, ids.rsf_program_id)
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
     LEFT JOIN p_rsf.view_current_entity_names_and_ids pnids ON pnids.rsf_pfcbl_id = ids.rsf_program_id
  WHERE ri.file_data IS NOT NULL AND rt.is_setup_template IS FALSE AND length(ri.file_data) > 0
  ORDER BY ri.reporting_asof_date;

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
-- View structure for view_rsf_setup_export_reporting_template_names
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_export_reporting_template_names";
CREATE VIEW "p_rsf"."view_rsf_setup_export_reporting_template_names" AS  SELECT ri.import_id,
    ri.template_id,
    ri.import_rsf_pfcbl_id,
    sn.sys_name,
    nids.pfcbl_name,
    ids.created_in_reporting_asof_date,
    first_template.reporting_asof_date AS first_reporting_asof_date,
    ri.reporting_asof_date AS current_reporting_asof_date,
    dates.next_reporting_asof_date,
    ids.deactivated_in_reporting_asof_date,
    concat(COALESCE(nids.id, 'SYSID'::text || nids.rsf_pfcbl_id), ' ', COALESCE(cnids.nickname, cnids.name, nids.nickname, nids.name)) AS entity_name,
        CASE
            WHEN ri.reporting_asof_date < first_template.reporting_asof_date THEN 0::numeric
            ELSE 1::numeric + (EXTRACT(year FROM ri.reporting_asof_date) * 4::numeric + EXTRACT(quarter FROM ri.reporting_asof_date)) - (EXTRACT(year FROM first_template.reporting_asof_date) * 4::numeric + EXTRACT(quarter FROM first_template.reporting_asof_date))
        END AS current_template_sequence_number
   FROM p_rsf.reporting_imports ri
     JOIN p_rsf.rsf_pfcbl_ids ids ON ids.rsf_pfcbl_id = ri.import_rsf_pfcbl_id
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = ri.import_rsf_pfcbl_id
     JOIN p_rsf.view_current_entity_names_and_ids nids ON nids.rsf_pfcbl_id = ri.import_rsf_pfcbl_id
     JOIN LATERAL ( SELECT clients_1.rsf_client_id AS primary_client_id
           FROM p_rsf.rsf_pfcbl_ids clients_1
          WHERE clients_1.rsf_facility_id = ids.rsf_pfcbl_id AND clients_1.pfcbl_category::text = 'client'::text
          ORDER BY clients_1.created_in_reporting_asof_date, clients_1.rsf_pfcbl_id) clients ON true
     JOIN p_rsf.view_current_entity_names_and_ids cnids ON cnids.rsf_pfcbl_id = clients.primary_client_id
     LEFT JOIN LATERAL ( SELECT grd.valid_reporting_date AS next_reporting_asof_date
           FROM p_rsf.rsf_pfcbl_generate_reporting_dates(v_rsf_pfcbl_id => ri.import_rsf_pfcbl_id, v_until_date => COALESCE(ids.deactivated_in_reporting_asof_date, now()::date)) grd(rsf_pfcbl_id, valid_reporting_date, reporting_sequence_rank, is_deactivated)
          WHERE grd.valid_reporting_date > ri.reporting_asof_date
          ORDER BY grd.valid_reporting_date
         LIMIT 1) dates ON true
     LEFT JOIN LATERAL ( SELECT rdc.reporting_asof_date,
            rdc.data_value AS template_reporting_asof_date
           FROM p_rsf.rsf_data_current rdc
             JOIN p_rsf.indicators ind ON ind.indicator_id = rdc.indicator_id AND ind.indicator_sys_category::text = 'reporting_date'::text
          WHERE rdc.rsf_pfcbl_id = clients.primary_client_id AND rdc.indicator_id = ind.indicator_id
          ORDER BY rdc.reporting_asof_date
         LIMIT 1) first_template ON true
  WHERE ids.pfcbl_category::text = 'facility'::text
  ORDER BY ri.reporting_asof_date, ri.import_id, sn.sys_name;

-- ----------------------------
-- View structure for view_rsf_pfcbl_id_family_tree
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pfcbl_id_family_tree";
CREATE VIEW "p_rsf"."view_rsf_pfcbl_id_family_tree" AS  SELECT ids.rsf_gpfcbl_family[1] AS to_family_rsf_pfcbl_id,
    'global'::character varying AS to_pfcbl_category,
    0::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 0 AND ids.rsf_gpfcbl_family[1] IS NOT NULL
UNION ALL
 SELECT ids.rsf_gpfcbl_family[2] AS to_family_rsf_pfcbl_id,
    'program'::character varying(255) AS to_pfcbl_category,
    1::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::character varying(255) AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 1 AND ids.rsf_gpfcbl_family[2] IS NOT NULL
UNION ALL
 SELECT ids.rsf_gpfcbl_family[3] AS to_family_rsf_pfcbl_id,
    'facility'::character varying(255) AS to_pfcbl_category,
    2::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::character varying(255) AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 2 AND ids.rsf_gpfcbl_family[3] IS NOT NULL
UNION ALL
 SELECT ids.rsf_gpfcbl_family[4] AS to_family_rsf_pfcbl_id,
    'client'::character varying(255) AS to_pfcbl_category,
    3::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::character varying(255) AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 3 AND ids.rsf_gpfcbl_family[4] IS NOT NULL
UNION ALL
 SELECT ids.rsf_gpfcbl_family[5] AS to_family_rsf_pfcbl_id,
    'borrower'::character varying(255) AS to_pfcbl_category,
    4::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::character varying(255) AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 4 AND ids.rsf_gpfcbl_family[5] IS NOT NULL
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'self'::text AS pfcbl_hierarchy
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
  WHERE ids.pfcbl_category_rank > 4;

-- ----------------------------
-- View structure for view_rsf_setup_template_headers
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_template_headers";
CREATE VIEW "p_rsf"."view_rsf_setup_template_headers" AS  SELECT sn.rsf_pfcbl_id,
    rt.template_id,
    sn.sys_name AS "SYSNAME",
    rt.template_name,
    fth.header_id,
        CASE
            WHEN fth.template_header_sheet_name ~ ':'::text THEN ( SELECT (regexp_matches(fth.template_header_sheet_name, '^(.*):(.*)$'::text))[1] AS regexp_matches)
            ELSE fth.template_header_sheet_name
        END AS template_header_sheet_name,
        CASE
            WHEN fth.template_header_sheet_name ~ ':'::text THEN ( SELECT (regexp_matches(fth.template_header_sheet_name, '^(.*):(.*)$'::text))[2] AS regexp_matches)
            ELSE NULL::text
        END AS template_header_sheet_index,
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
   FROM p_rsf.rsf_setup_template_headers fth
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = fth.rsf_pfcbl_id
     JOIN p_rsf.reporting_templates rt ON rt.template_id = fth.template_id
     LEFT JOIN p_rsf.indicators ind ON ind.indicator_id = fth.map_indicator_id
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.formula_id = fth.map_formula_id
     LEFT JOIN p_rsf.indicators find ON find.indicator_id = indf.indicator_id
     LEFT JOIN p_rsf.indicator_check_formulas icf ON icf.check_formula_id = fth.map_check_formula_id
     LEFT JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = icf.indicator_check_id
  ORDER BY sn.rsf_pfcbl_id, fth.template_header_sheet_name, fth.template_header, fth.action;

-- ----------------------------
-- View structure for view_rsf_setup_archive
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_archive";
CREATE VIEW "p_rsf"."view_rsf_setup_archive" AS  SELECT nids.rsf_pfcbl_id,
    nids.sys_name,
    nids.reporting_asof_date,
    nids.pfcbl_name,
    nids.id,
    'rsf_setup_checks'::text AS settings_source,
    settings.settings_value
   FROM p_rsf.rsf_data_current_names_and_ids nids
     JOIN LATERAL ( SELECT jsonb_agg(x.*) AS settings_value
           FROM ( SELECT rsc.rsf_pfcbl_id,
                    rsc.check_formula_id,
                    rsc.indicator_check_id,
                    rsc.rsf_program_id,
                    rsc.rsf_facility_id,
                    rsc.is_subscribed,
                    rsc.is_auto_subscribed,
                    rsc.subscription_comments,
                    rsc.comments_user_id,
                    rsc.auto_subscribed_by_reporting_cohort_id
                   FROM p_rsf.rsf_setup_checks rsc
                  WHERE rsc.rsf_pfcbl_id = nids.rsf_pfcbl_id) x) settings ON true
  WHERE nids.pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text])
UNION ALL
 SELECT nids.rsf_pfcbl_id,
    nids.sys_name,
    nids.reporting_asof_date,
    nids.pfcbl_name,
    nids.id,
    'rsf_setup_checks_config'::text AS settings_source,
    settings.settings_value
   FROM p_rsf.rsf_data_current_names_and_ids nids
     JOIN LATERAL ( SELECT jsonb_agg(x.*) AS settings_value
           FROM ( SELECT rsc.config_id,
                    rsc.rsf_pfcbl_id,
                    rsc.for_indicator_id,
                    rsc.indicator_check_id,
                    rsc.check_formula_id,
                    rsc.rsf_program_id,
                    rsc.rsf_facility_id,
                    rsc.config_auto_resolve,
                    rsc.config_check_class,
                    rsc.config_threshold,
                    rsc.config_apply_asof_date,
                    rsc.config_comments,
                    rsc.comments_user_id,
                    rsc.auto_subscribed_by_reporting_cohort_id
                   FROM p_rsf.rsf_setup_checks_config rsc
                  WHERE rsc.rsf_pfcbl_id = nids.rsf_pfcbl_id) x) settings ON true
  WHERE nids.pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text])
UNION ALL
 SELECT nids.rsf_pfcbl_id,
    nids.sys_name,
    nids.reporting_asof_date,
    nids.pfcbl_name,
    nids.id,
    'rsf_setup_indicators'::text AS settings_source,
    settings.settings_value
   FROM p_rsf.rsf_data_current_names_and_ids nids
     JOIN LATERAL ( SELECT jsonb_agg(x.*) AS settings_value
           FROM ( SELECT rsc.indicator_id,
                    rsc.formula_id,
                    rsc.rsf_program_id,
                    rsc.rsf_facility_id,
                    rsc.is_subscribed,
                    rsc.is_auto_subscribed,
                    rsc.sort_preference,
                    rsc.subscription_comments,
                    rsc.comments_user_id,
                    rsc.options_group_id,
                    rsc.formula_calculation_unit
                   FROM p_rsf.rsf_setup_indicators rsc
                  WHERE rsc.rsf_pfcbl_id = nids.rsf_pfcbl_id) x) settings ON true
  WHERE nids.pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text])
UNION ALL
 SELECT nids.rsf_pfcbl_id,
    nids.sys_name,
    nids.reporting_asof_date,
    nids.pfcbl_name,
    nids.id,
    'rsf_setup_template_headers'::text AS settings_source,
    settings.settings_value
   FROM p_rsf.rsf_data_current_names_and_ids nids
     JOIN LATERAL ( SELECT jsonb_agg(x.*) AS settings_value
           FROM ( SELECT rsc.template_id,
                    rsc.header_id,
                    rsc.template_header_sheet_name,
                    rsc.template_header,
                    rsc.action,
                    rsc.action_mapping,
                    rsc.comment,
                    rsc.map_indicator_id,
                    rsc.map_formula_id,
                    rsc.map_check_formula_id,
                    rsc.template_header_full_normalized
                   FROM p_rsf.rsf_setup_template_headers rsc
                  WHERE rsc.rsf_pfcbl_id = nids.rsf_pfcbl_id) x) settings ON true
  WHERE nids.pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text])
UNION ALL
 SELECT nids.rsf_pfcbl_id,
    nids.sys_name,
    nids.reporting_asof_date,
    nids.pfcbl_name,
    nids.id,
    'users.permissions'::text AS settings_source,
    settings.settings_value
   FROM p_rsf.rsf_data_current_names_and_ids nids
     JOIN LATERAL ( SELECT jsonb_agg(x.*) AS settings_value
           FROM ( SELECT rsc.account_id,
                    rsc.granted,
                    rsc.denied,
                    rsc.notes
                   FROM users.permissions rsc
                  WHERE rsc.rsf_pfcbl_id = nids.rsf_pfcbl_id) x) settings ON true
  WHERE nids.pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text])
UNION ALL
 SELECT nids.rsf_pfcbl_id,
    nids.sys_name,
    nids.reporting_asof_date,
    nids.pfcbl_name,
    nids.id,
    'rsf_data_checks_archive'::text AS settings_source,
    settings.settings_value
   FROM p_rsf.rsf_data_current_names_and_ids nids
     JOIN LATERAL ( SELECT jsonb_agg(chk.*) AS settings_value
           FROM p_rsf.view_rsf_pfcbl_id_family_tree ft
             JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
             JOIN LATERAL ( SELECT dca.archive_id,
                    dca.archive_time,
                    dca.sys_name,
                    NULL::text AS rsf_pfcbl_id,
                    dca.indicator_id,
                    dca.indicator_check_id,
                    dca.check_formula_id,
                    dca.check_asof_date,
                    dca.check_status,
                    dca.status_time,
                    dca.check_status_user_id,
                    dca.check_status_comment,
                    dca.check_message,
                    dca.data_sys_flags,
                    dca.data_value_unit,
                    dca.data_correction_date
                   FROM p_rsf.rsf_data_checks_archive dca
                  WHERE dca.sys_name = sn.sys_name OR dca.sys_name ~~ (sn.sys_name || '%'::text)
                UNION ALL
                 SELECT cae.evaluation_id AS archive_id,
                    now() AS archive_time,
                    cae.archive_sys_name AS sys_name,
                    NULL::text AS rsf_pfcbl_id,
                    cae.indicator_id,
                    cae.indicator_check_id,
                    cae.check_formula_id,
                    cae.check_asof_date,
                    cae.check_status,
                    cae.status_time,
                    cae.check_status_user_id,
                    cae.check_status_comment,
                    cae.check_message,
                    cae.data_sys_flags,
                    cae.data_value_unit,
                    cae.data_correction_date
                   FROM p_rsf.rsf_data_checks cae
                     JOIN p_rsf.view_account_info vai ON vai.account_id = cae.check_status_user_id
                  WHERE cae.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id AND cae.archive_sys_name IS NOT NULL AND cae.data_value_unit IS NOT NULL AND vai.is_system_account = false AND (cae.check_formula_id IS NULL OR (EXISTS ( SELECT true
                           FROM p_rsf.indicator_check_formulas icf
                          WHERE icf.check_formula_id = cae.check_formula_id)))) chk ON true
          WHERE ft.from_rsf_pfcbl_id = nids.rsf_pfcbl_id AND
                CASE
                    WHEN nids.pfcbl_category = ANY (ARRAY['global'::text, 'program'::text]) THEN ft.pfcbl_hierarchy = 'self'::text
                    ELSE ft.pfcbl_hierarchy <> 'parent'::text
                END) settings ON true
  WHERE nids.pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text]);

-- ----------------------------
-- View structure for view_rsf_pf_calculation_requirements
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pf_calculation_requirements";
CREATE VIEW "p_rsf"."view_rsf_pf_calculation_requirements" AS  SELECT ids.rsf_pf_id AS from_rsf_pf_id,
    cpid.to_calculate_pf_id,
    ids.rsf_pfcbl_id,
    rsi.indicator_id,
    ind.data_category,
    ind.pfcbl_rank AS data_category_rank,
    ind.unit_fx_indicator_id,
    ind.unit_fx_method,
    ids.created_in_reporting_asof_date,
    rsi.formula_id,
    COALESCE(ind.is_periodic_or_flow_reporting, false) AS is_periodic,
    COALESCE(indf.formula_calculation_rank::integer, 0) AS formula_calculation_rank,
    COALESCE(indf.has_reporting_parameters, false) OR COALESCE(indf.has_timeseries_parameters, false) AS is_validate_always,
    COALESCE(indf.has_no_parameters, true) AS has_no_parameters,
    f.parent_id AS parent_pf_id,
    f.parent_rank AS parent_pf_rank,
    ids.pfcbl_category_rank AS pfcbl_rank,
    COALESCE(indf.formula_pfcbl_rank_range, ARRAY[ind.pfcbl_rank]) AS formula_pfcbl_rank_range,
    COALESCE(indf.formula_grouping_pfcbl_rank, ind.pfcbl_rank) AS formula_calculate_from_pfcbl_rank
   FROM p_rsf.rsf_pfcbl_ids ids
     CROSS JOIN LATERAL ( VALUES (ids.rsf_gpfcbl_family[1],0), (ids.rsf_gpfcbl_family[2],1), (ids.rsf_gpfcbl_family[3],2)) f(parent_id, parent_rank)
     JOIN p_rsf.rsf_setup_indicators rsi ON rsi.rsf_pfcbl_id = f.parent_id
     JOIN p_rsf.indicators ind ON ind.indicator_id = rsi.indicator_id
     CROSS JOIN LATERAL ( VALUES (ids.rsf_gpfcbl_family[LEAST(2, ind.pfcbl_rank::integer) + 1])) cpid(to_calculate_pf_id)
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.formula_id = rsi.formula_id
  WHERE ids.pfcbl_category_rank <= 2 AND rsi.is_subscribed IS TRUE AND (rsi.formula_id IS NOT NULL OR ind.unit_fx_indicator_id IS NOT NULL);

-- ----------------------------
-- View structure for view_rsf_setup_template_header_actions
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_setup_template_header_actions";
CREATE VIEW "p_rsf"."view_rsf_setup_template_header_actions" AS  WITH family_headers AS (
         SELECT DISTINCT ON (ft.from_rsf_pfcbl_id, fth_1.template_id, fth_1.template_header_full_normalized) ft.from_rsf_pfcbl_id AS rsf_pfcbl_id,
            ft.to_family_rsf_pfcbl_id,
            fth_1.template_id,
            fth_1.template_header_full_normalized,
            ft.to_pfcbl_category AS action_level,
            ft.from_pfcbl_rank - ft.to_pfcbl_rank AS action_distance,
            ids.rsf_program_id
           FROM p_rsf.view_rsf_pfcbl_id_family_tree ft
             JOIN p_rsf.rsf_pfcbl_ids ids ON ids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
             JOIN p_rsf.rsf_setup_template_headers fth_1 ON fth_1.rsf_pfcbl_id = ids.rsf_pfcbl_id
          WHERE (ft.from_pfcbl_category::text = ANY (ARRAY['global'::character varying, 'program'::character varying, 'facility'::character varying]::text[])) AND ft.to_pfcbl_rank <= ft.from_pfcbl_rank
          ORDER BY ft.from_rsf_pfcbl_id, fth_1.template_id, fth_1.template_header_full_normalized, ft.to_pfcbl_rank DESC
        ), all_headers AS (
         SELECT fh.rsf_pfcbl_id,
            fh.to_family_rsf_pfcbl_id,
            fh.template_id,
            fh.template_header_full_normalized,
            fh.action_level,
            fh.action_distance
           FROM family_headers fh
        UNION ALL
         SELECT DISTINCT ids.rsf_pfcbl_id,
            fth_1.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
            fth_1.template_id,
            fth_1.template_header_full_normalized,
            'relative'::text AS action_level,
            3 AS action_distance
           FROM p_rsf.rsf_pfcbl_ids ids
             JOIN p_rsf.rsf_setup_template_headers fth_1 ON NOT fth_1.rsf_program_id IS DISTINCT FROM ids.rsf_program_id
          WHERE (ids.pfcbl_category::text = ANY (ARRAY['global'::character varying, 'program'::character varying, 'facility'::character varying]::text[])) AND NOT (EXISTS ( SELECT fh.rsf_pfcbl_id,
                    fh.to_family_rsf_pfcbl_id,
                    fh.template_id,
                    fh.template_header_full_normalized,
                    fh.action_level,
                    fh.action_distance,
                    fh.rsf_program_id
                   FROM family_headers fh
                  WHERE fh.rsf_pfcbl_id = ids.rsf_pfcbl_id AND fh.template_id = fth_1.template_id AND fh.template_header_full_normalized = fth_1.template_header_full_normalized))
          GROUP BY ids.rsf_pfcbl_id, fth_1.rsf_pfcbl_id, fth_1.template_id, fth_1.template_header_full_normalized, fth_1.action_mapping
         HAVING count(*) > 1
        )
 SELECT headers.rsf_pfcbl_id,
    headers.template_id,
    fth.header_id,
    fth."SYSNAME",
    fth.template_name,
    fth.template_header_sheet_name AS template_header_section_name,
    fth.template_header_sheet_index AS template_header_section_index,
    fth.template_header,
    fth.action,
    fth.comment,
    fth.map_indicator_id,
    fth.indicator_name,
    fth.map_formula_id,
    fth.calculation_formula,
    fth.map_check_formula_id,
    fth.check_formula,
    headers.action_level,
    sn.sys_name AS action_source
   FROM all_headers headers
     JOIN p_rsf.view_rsf_setup_template_headers fth ON fth.rsf_pfcbl_id = headers.to_family_rsf_pfcbl_id AND fth.template_id = headers.template_id AND fth.template_header_full_normalized = headers.template_header_full_normalized
     JOIN p_rsf.view_rsf_pfcbl_id_current_sys_names sn ON sn.rsf_pfcbl_id = fth.rsf_pfcbl_id;

-- ----------------------------
-- View structure for view_rsf_pf_calculation_evaluations_required
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pf_calculation_evaluations_required";
CREATE VIEW "p_rsf"."view_rsf_pf_calculation_evaluations_required" AS  SELECT sis.from_rsf_pf_id,
    rdates.quarter_end_date AS from_reporting_asof_date,
    0 AS from_reporting_calculation_rank,
    calc.rsf_pfcbl_id AS calculate_rsf_pfcbl_id,
    sis.indicator_id AS calculate_indicator_id,
    rdates.quarter_end_date AS calculate_asof_date,
    calc.rsf_pf_id AS to_rsf_pf_id,
    sis.formula_calculation_rank AS to_formula_calculation_rank
   FROM p_rsf.reporting_dates rdates
     JOIN p_rsf.view_rsf_pf_calculation_requirements sis ON sis.created_in_reporting_asof_date <= rdates.quarter_end_date
     JOIN p_rsf.rsf_pfcbl_ids calc ON calc.rsf_pf_id = sis.parent_pf_id AND calc.pfcbl_category_rank = sis.data_category_rank
  WHERE rdates.quarter_end_date >= calc.created_in_reporting_asof_date AND (calc.deactivated_in_reporting_asof_date IS NULL OR calc.deactivated_in_reporting_asof_date >= rdates.quarter_end_date OR NOT sis.unit_fx_method IS DISTINCT FROM 'fx'::text) AND ((sis.is_periodic IS TRUE OR sis.is_validate_always IS TRUE) AND NOT (EXISTS ( SELECT true
           FROM p_rsf.rsf_data_calculation_validations dcv
          WHERE dcv.rsf_pfcbl_id = calc.rsf_pfcbl_id AND dcv.indicator_id = sis.indicator_id AND dcv.calculation_asof_date = rdates.quarter_end_date)) OR sis.unit_fx_method IS DISTINCT FROM 'parameter'::text AND (sis.formula_id IS NOT NULL OR sis.formula_id IS NULL AND sis.unit_fx_indicator_id IS NOT NULL AND sis.is_periodic IS FALSE AND (( SELECT NULLIF(rdc.data_value, '0'::text) AS "nullif"
           FROM p_rsf.rsf_data_current rdc
          WHERE rdc.rsf_pfcbl_id = calc.rsf_pfcbl_id AND (rdc.indicator_id = ANY (ARRAY[sis.unit_fx_indicator_id, sis.indicator_id])) AND rdc.reporting_asof_date <= rdates.quarter_end_date
          ORDER BY rdc.reporting_asof_date DESC, (NULLIF(rdc.data_value, '0'::text) IS NOT NULL) DESC
         LIMIT 1)) IS NOT NULL) AND (EXISTS ( SELECT true
           FROM p_rsf.rsf_data_current_fx fx
             JOIN p_rsf.rsf_data_current fxrates ON fxrates.data_id = fx.fx_data_id
          WHERE fx.rsf_pfcbl_id = calc.rsf_pfcbl_id AND fx.indicator_id = sis.indicator_id AND fx.reporting_asof_date <= rdates.quarter_end_date AND NOT (EXISTS ( SELECT true
                   FROM p_rsf.rsf_data_calculation_validations dvc
                  WHERE dvc.rsf_pfcbl_id = calc.rsf_pfcbl_id AND dvc.indicator_id = sis.indicator_id AND dvc.calculation_asof_date = rdates.quarter_end_date AND dvc.validation_time > fxrates.data_time)))) OR (EXISTS ( SELECT true
           FROM p_rsf.rsf_data_current cdata
          WHERE cdata.rsf_pfcbl_id = calc.rsf_pfcbl_id AND cdata.indicator_id = sis.indicator_id AND cdata.reporting_asof_date = rdates.quarter_end_date AND cdata.is_calculated IS FALSE AND NOT (EXISTS ( SELECT true
                   FROM p_rsf.rsf_data_calculation_validations dcv
                  WHERE dcv.data_id = cdata.data_id AND dcv.calculation_asof_date = cdata.reporting_asof_date AND dcv.validation_time > cdata.data_time)))))
UNION ALL
 SELECT sis.from_rsf_pf_id,
    rdates.quarter_end_date AS from_reporting_asof_date,
    NULL::smallint AS from_reporting_calculation_rank,
    calc.rsf_pfcbl_id AS calculate_rsf_pfcbl_id,
    sis.indicator_id AS calculate_indicator_id,
    calc.created_in_reporting_asof_date AS calculate_asof_date,
    calc.rsf_pf_id AS to_rsf_pf_id,
    sis.formula_calculation_rank AS to_formula_calculation_rank
   FROM p_rsf.reporting_dates rdates
     CROSS JOIN p_rsf.view_rsf_pf_calculation_requirements sis
     JOIN p_rsf.rsf_pfcbl_ids calc ON calc.rsf_pf_id = sis.parent_pf_id AND calc.pfcbl_category_rank = sis.data_category_rank
  WHERE NOT (EXISTS ( SELECT true
           FROM p_rsf.rsf_data_calculation_validations initdata
          WHERE initdata.rsf_pfcbl_id = calc.rsf_pfcbl_id AND initdata.indicator_id = sis.indicator_id AND initdata.calculation_asof_date = calc.created_in_reporting_asof_date))
UNION ALL
 SELECT sis.from_rsf_pf_id,
    pdata.reporting_asof_date AS from_reporting_asof_date,
    0 AS from_reporting_calculation_rank,
    calc.rsf_pfcbl_id AS calculate_rsf_pfcbl_id,
    sis.indicator_id AS calculate_indicator_id,
    pdata.reporting_asof_date AS calculate_asof_date,
    calc.rsf_pf_id AS to_rsf_pf_id,
    sis.formula_calculation_rank AS to_formula_calculation_rank
   FROM p_rsf.view_rsf_pf_calculation_requirements sis
     JOIN p_rsf.rsf_pfcbl_ids calc ON calc.rsf_pf_id = sis.from_rsf_pf_id AND calc.pfcbl_category_rank = sis.data_category_rank
     JOIN p_rsf.rsf_data_current pdata ON pdata.rsf_pfcbl_id = calc.rsf_pfcbl_id AND pdata.indicator_id = sis.unit_fx_indicator_id
  WHERE sis.unit_fx_indicator_id IS NOT NULL AND sis.unit_fx_method = 'parameter'::text AND sis.formula_id IS NULL AND sis.has_no_parameters IS TRUE AND pdata.reporting_asof_date >= calc.created_in_reporting_asof_date AND NOT (EXISTS ( SELECT true
           FROM p_rsf.rsf_data_calculation_validations dvc
          WHERE dvc.rsf_pfcbl_id = calc.rsf_pfcbl_id AND dvc.indicator_id = sis.indicator_id AND dvc.calculation_asof_date = pdata.reporting_asof_date AND dvc.validation_time > pdata.data_time))
UNION ALL
 SELECT calcs.from_rsf_pf_id,
    calcs.reporting_asof_date AS from_reporting_asof_date,
    calcs.trigger_calculation_rank AS from_reporting_calculation_rank,
    calcs.calculate_rsf_pfcbl_id,
    calcs.calculate_indicator_id,
    calcs.calculate_asof_date,
    calcs.to_rsf_pf_id,
    calcs.formula_calculation_rank AS to_formula_calculation_rank
   FROM ( SELECT ctp.from_rsf_pf_id,
            calc.rsf_pf_id AS to_rsf_pf_id,
            calc.rsf_pfcbl_id AS calculate_rsf_pfcbl_id,
            ctp.indicator_id AS calculate_indicator_id,
            rdates.reporting_asof_date,
            pdata.reporting_asof_date AS calculate_asof_date,
            rdates.reporting_calculation_rank AS trigger_calculation_rank,
            ctp.formula_calculation_rank,
            max(pdata.data_time) AS latest_parameter_time
           FROM p_rsf.view_rsf_pf_calculation_parameter_requirements ctp
             JOIN p_rsf.reporting_cohorts rdates ON rdates.reporting_rsf_pfcbl_id = ctp.to_parameter_pf_id
             JOIN p_rsf.indicator_formula_parameters ifp ON ifp.formula_id = ctp.formula_id
             JOIN p_rsf.rsf_data_current pdata ON pdata.data_time = rdates.reporting_time AND pdata.indicator_id = ifp.parameter_indicator_id
             JOIN p_rsf.view_rsf_pfcbl_id_family_tree ft ON ft.from_rsf_pfcbl_id = pdata.rsf_pfcbl_id AND ft.to_pfcbl_rank = ctp.data_category_rank
             JOIN p_rsf.rsf_pfcbl_ids calc ON calc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
          WHERE pdata.reporting_asof_date >= calc.created_in_reporting_asof_date AND ctp.formula_id IS NOT NULL AND rdates.reporting_calculation_rank < ctp.formula_calculation_rank AND (ft.from_pfcbl_rank >= calc.pfcbl_category_rank OR NOT ctp.unit_fx_method IS DISTINCT FROM 'fx'::text OR calc.deactivated_in_reporting_asof_date IS NULL OR calc.deactivated_in_reporting_asof_date >= pdata.reporting_asof_date)
          GROUP BY ctp.from_rsf_pf_id, calc.rsf_pf_id, calc.rsf_pfcbl_id, ctp.indicator_id, ctp.formula_calculation_rank, rdates.reporting_calculation_rank, pdata.reporting_asof_date, rdates.reporting_asof_date) calcs
  WHERE NOT (EXISTS ( SELECT true
           FROM p_rsf.rsf_data_calculation_validations dcv
          WHERE dcv.rsf_pfcbl_id = calcs.calculate_rsf_pfcbl_id AND dcv.indicator_id = calcs.calculate_indicator_id AND dcv.calculation_asof_date = calcs.calculate_asof_date AND dcv.validation_time > calcs.latest_parameter_time));

-- ----------------------------
-- View structure for rsf_data_currentest_names_and_ids
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."rsf_data_currentest_names_and_ids";
CREATE VIEW "p_rsf"."rsf_data_currentest_names_and_ids" AS  SELECT updates.rsf_pfcbl_id,
    updates.reporting_asof_date,
    TRIM(BOTH FROM max(regexp_replace(cd.data_value, '^(.*)#[[:digit:]]+$'::text, '\1'::text, 'g'::text)) FILTER (WHERE ind.indicator_sys_category::text = 'id'::text)) AS id,
    max(cd.data_value) FILTER (WHERE ind.indicator_sys_category::text = 'rank_id'::text) AS rank_id,
    max(cd.data_value) FILTER (WHERE ind.indicator_sys_category::text = 'tranche_id'::text) AS tranche_id,
    TRIM(BOTH FROM max(regexp_replace(regexp_replace(cd.data_value, '[^A-Za-z0-9[:space:]''&.-]'::text, ' '::text, 'g'::text), '[[:space:]]{2,}'::text, ' '::text, 'g'::text)) FILTER (WHERE ind.indicator_sys_category::text = 'name'::text)) AS name,
    max(cd.data_value) FILTER (WHERE ind.indicator_sys_category::text = 'nickname'::text) AS nickname,
    ind.data_category,
        CASE
            WHEN max(cd.data_value) FILTER (WHERE ind.indicator_sys_category::text = 'rank_id'::text AND ind.data_category::text = 'loan'::text) IS NULL AND max(cd.data_value) FILTER (WHERE ind.indicator_sys_category::text = 'id'::text) IS NULL AND max(cd.data_value) FILTER (WHERE ind.indicator_sys_category::text = 'name'::text) IS NULL THEN concat(ind.data_category::text || ':SYSID'::text, updates.rsf_pfcbl_id)
            ELSE concat(ind.data_category::text || ':'::text, COALESCE('RANK'::text || max(cd.data_value) FILTER (WHERE ind.indicator_sys_category::text = 'rank_id'::text AND ind.data_category::text = 'loan'::text), TRIM(BOTH FROM max(regexp_replace(regexp_replace(cd.data_value, '[^A-Za-z0-9[:space:]''&.-]'::text, ' '::text, 'g'::text), '[[:space:]]{2,}'::text, ' '::text, 'g'::text)) FILTER (WHERE ind.indicator_sys_category::text = 'name'::text))), ' ('::text || TRIM(BOTH FROM max(regexp_replace(cd.data_value, '^(.*)#[[:digit:]]+$'::text, '\1'::text, 'g'::text)) FILTER (WHERE ind.indicator_sys_category::text = 'id'::text) || ')'::text))
        END AS pfcbl_name,
    max(cd.data_id) FILTER (WHERE ind.indicator_sys_category::text = 'entity_reporting'::text) AS entity_reporting_id
   FROM ( SELECT nidrdc.rsf_pfcbl_id,
            nidrdc.reporting_asof_date,
            max(ind_1.pfcbl_rank) AS data_category_rank
           FROM p_rsf.indicators ind_1
             JOIN p_rsf.rsf_data_current nidrdc ON nidrdc.indicator_id = ind_1.indicator_id
          WHERE ind_1.indicator_sys_category::text = ANY (ARRAY['id'::character varying, 'rank_id'::character varying, 'name'::character varying, 'nickname'::character varying, 'tranche_id'::character varying]::text[])
          GROUP BY nidrdc.rsf_pfcbl_id, nidrdc.reporting_asof_date) updates
     JOIN p_rsf.indicators ind ON ind.pfcbl_rank = updates.data_category_rank AND (ind.indicator_sys_category::text = ANY (ARRAY['id'::character varying, 'rank_id'::character varying, 'name'::character varying, 'nickname'::character varying, 'tranche_id'::character varying, 'entity_reporting'::character varying]::text[]))
     JOIN LATERAL ( SELECT rdc.data_value,
            rdc.data_id
           FROM p_rsf.rsf_data_current rdc
          WHERE rdc.rsf_pfcbl_id = updates.rsf_pfcbl_id AND rdc.indicator_id = ind.indicator_id AND rdc.reporting_asof_date <= updates.reporting_asof_date
          ORDER BY rdc.reporting_asof_date DESC
         LIMIT 1) cd ON true
  GROUP BY updates.rsf_pfcbl_id, updates.reporting_asof_date, ind.data_category;

-- ----------------------------
-- View structure for view_rsf_pfcbl_id_family_tree_optimized
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."view_rsf_pfcbl_id_family_tree_optimized";
CREATE VIEW "p_rsf"."view_rsf_pfcbl_id_family_tree_optimized" AS  SELECT ids.rsf_gpfcbl_family[1] AS to_family_rsf_pfcbl_id,
    'global'::text AS to_pfcbl_category,
    0::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category::text AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 0 AND ids.rsf_gpfcbl_family[1] IS NOT NULL
UNION ALL
 SELECT ids.rsf_gpfcbl_family[2] AS to_family_rsf_pfcbl_id,
    'program'::text AS to_pfcbl_category,
    1::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category::text AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 1 AND ids.rsf_gpfcbl_family[2] IS NOT NULL
UNION ALL
 SELECT ids.rsf_gpfcbl_family[3] AS to_family_rsf_pfcbl_id,
    'facility'::text AS to_pfcbl_category,
    2::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category::text AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 2 AND ids.rsf_gpfcbl_family[3] IS NOT NULL
UNION ALL
 SELECT ids.rsf_gpfcbl_family[4] AS to_family_rsf_pfcbl_id,
    'client'::text AS to_pfcbl_category,
    3::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category::text AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 3 AND ids.rsf_gpfcbl_family[4] IS NOT NULL
UNION ALL
 SELECT ids.rsf_gpfcbl_family[5] AS to_family_rsf_pfcbl_id,
    'borrower'::text AS to_pfcbl_category,
    4::smallint AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category::text AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'parent'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 4 AND ids.rsf_gpfcbl_family[5] IS NOT NULL
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category::text AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_pfcbl_id AS from_rsf_pfcbl_id,
    ids.pfcbl_category AS from_pfcbl_category,
    ids.pfcbl_category_rank AS from_pfcbl_rank,
    'self'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category::text AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_program_id AS from_rsf_pfcbl_id,
    'program'::text AS from_pfcbl_category,
    1::smallint AS from_pfcbl_rank,
    'child'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 1
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category::text AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_facility_id AS from_rsf_pfcbl_id,
    'facility'::text AS from_pfcbl_category,
    2::smallint AS from_pfcbl_rank,
    'child'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 2
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category::text AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_client_id AS from_rsf_pfcbl_id,
    'client'::text AS from_pfcbl_category,
    3::smallint AS from_pfcbl_rank,
    'child'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 3
UNION ALL
 SELECT ids.rsf_pfcbl_id AS to_family_rsf_pfcbl_id,
    ids.pfcbl_category::text AS to_pfcbl_category,
    ids.pfcbl_category_rank AS to_pfcbl_rank,
    ids.rsf_borrower_id AS from_rsf_pfcbl_id,
    'borrower'::text AS from_pfcbl_category,
    4::smallint AS from_pfcbl_rank,
    'child'::text AS pfcbl_hierarchy
   FROM p_rsf.rsf_pfcbl_ids ids
  WHERE ids.pfcbl_category_rank > 4;

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
-- View structure for error_check_calculation_evaluations
-- ----------------------------
DROP VIEW IF EXISTS "p_rsf"."error_check_calculation_evaluations";
CREATE VIEW "p_rsf"."error_check_calculation_evaluations" AS  SELECT dce.rsf_pfcbl_id,
    ind.indicator_name,
    ind.data_category,
    ids.pfcbl_category,
    ids.rsf_pf_id IS DISTINCT FROM dce.rsf_pf_id AS mismatch_rsf_pf_id,
    ind.data_category::text IS DISTINCT FROM ids.pfcbl_category::text AS mismatch_category,
    COALESCE(indf.formula_calculation_rank::integer, 0) IS DISTINCT FROM dce.formula_calculation_rank AS mismatch_rank,
    dce.calculation_asof_date,
    dce.indicator_id,
    ids.rsf_pf_id AS ids_rsf_pf_id,
    dce.rsf_pf_id AS eval_rsf_pf_id
   FROM p_rsf.rsf_data_calculation_evaluations dce
     JOIN p_rsf.indicators ind ON ind.indicator_id = dce.indicator_id
     JOIN p_rsf.rsf_pfcbl_ids ids ON ids.rsf_pfcbl_id = dce.rsf_pfcbl_id
     LEFT JOIN p_rsf.rsf_setup_indicators rsi ON rsi.rsf_pfcbl_id = dce.rsf_pf_id AND rsi.indicator_id = ind.indicator_id AND rsi.is_subscribed IS TRUE AND (rsi.formula_id IS NOT NULL OR ind.unit_fx_indicator_id IS NOT NULL)
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.formula_id = rsi.formula_id
  WHERE dce.rsf_pf_id IS DISTINCT FROM ids.rsf_pf_id OR ind.pfcbl_rank IS DISTINCT FROM ids.pfcbl_category_rank OR rsi.rsf_pfcbl_id IS NULL OR COALESCE(indf.formula_calculation_rank::integer, 0) IS DISTINCT FROM dce.formula_calculation_rank;

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
    ind.is_system OR ind.is_required AS is_system_indicator,
    indf.formula_id,
        CASE
            WHEN pfi.formula_calculation_unit IS NOT NULL THEN pfi.formula_calculation_unit
            WHEN indf.formula_id IS NOT NULL THEN ind.data_unit::text
            ELSE NULL::text
        END AS formula_calculation_unit,
    indf.formula_calculation_rank,
    indf.formula_id IS NOT NULL OR ind.unit_fx_indicator_id IS NOT NULL AS is_calculated,
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
    ids.deactivated_in_reporting_asof_date,
    ind.is_periodic_or_flow_reporting,
    ind.unit_fx_method,
    ind.unit_fx_source,
    ind.unit_fx_indicator_id,
    indf.overwrite AS formula_overwrite,
    ids.rsf_pf_id
   FROM p_rsf.rsf_pfcbl_ids ids
     CROSS JOIN p_rsf.indicators ind
     LEFT JOIN p_rsf.rsf_setup_indicators pfi ON (pfi.rsf_pfcbl_id = ids.rsf_facility_id OR pfi.rsf_pfcbl_id = ids.rsf_program_id OR pfi.rsf_pfcbl_id = 0) AND pfi.indicator_id = ind.indicator_id
     LEFT JOIN p_rsf.indicator_formulas indf ON indf.indicator_id = COALESCE(ind.unit_fx_indicator_id, ind.indicator_id) AND (pfi.indicator_id IS NULL AND indf.is_primary_default IS TRUE OR NOT pfi.formula_id IS DISTINCT FROM indf.formula_id);

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
    ic.auto_subscribe IS TRUE AND bool_and(sis.is_subscribed IS TRUE) FILTER (WHERE cfp.is_calculation_trigger_parameter IS TRUE) AS is_auto_monitorable,
    ic.check_type ~ 'contract'::text AND bool_and(sis.is_subscribed) FILTER (WHERE sis.is_calculated IS FALSE AND sis.is_system_indicator IS FALSE AND sis.data_category::text = 'facility'::text) AS is_contract_monitorable,
    array_agg(cfp.parameter_indicator_id ORDER BY cfp.parameter_indicator_id) FILTER (WHERE sis.is_subscribed IS FALSE) AS unmonitored_parameter_ids,
    NOT sis.pfcbl_category::text IS DISTINCT FROM ic.check_pfcbl_category OR sis.pfcbl_category::text = 'facility'::text AND (ic.check_pfcbl_category = ANY (ARRAY['client'::text, 'borrower'::text, 'loan'::text])) AS filter_category_manager
   FROM p_rsf.view_rsf_setup_indicator_subscriptions sis
     JOIN p_rsf.indicator_check_formula_parameters cfp ON cfp.parameter_indicator_id = sis.indicator_id
     JOIN p_rsf.indicator_checks ic ON ic.indicator_check_id = cfp.indicator_check_id
  GROUP BY sis.rsf_pfcbl_id, sis.pfcbl_category, sis.filter_matched_pfcbl_indicators, cfp.indicator_check_id, cfp.check_formula_id, ic.check_class, ic.check_type, ic.check_name, ic.check_pfcbl_category, ic.auto_subscribe;

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
SELECT setval('"p_rsf"."exporting_cohorts_exporting_cohort_id_seq"', 680, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."import_templates_import_id_seq"
OWNED BY "p_rsf"."reporting_imports"."import_id";
SELECT setval('"p_rsf"."import_templates_import_id_seq"', 103773, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_check_formulas_check_formula_id_seq"
OWNED BY "p_rsf"."indicator_check_formulas"."check_formula_id";
SELECT setval('"p_rsf"."indicator_check_formulas_check_formula_id_seq"', 259, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_check_guidance_guidance_id_seq"
OWNED BY "p_rsf"."!dep-indicator_check_guidance"."indicator_check_guidance_id";
SELECT setval('"p_rsf"."indicator_check_guidance_guidance_id_seq"', 141, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_checks_check_id_seq"
OWNED BY "p_rsf"."indicator_checks"."indicator_check_id";
SELECT setval('"p_rsf"."indicator_checks_check_id_seq"', 48008, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_classifications_classification_id_seq"
OWNED BY "p_rsf"."indicator_classifications"."classification_id";
SELECT setval('"p_rsf"."indicator_classifications_classification_id_seq"', 9, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicator_formulas_formula_id_seq"
OWNED BY "p_rsf"."indicator_formulas"."formula_id";
SELECT setval('"p_rsf"."indicator_formulas_formula_id_seq"', 538, true);

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
SELECT setval('"p_rsf"."indicator_option_groups_option_group_id_seq"', 40, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."indicators_indicator_id_seq"
OWNED BY "p_rsf"."indicators"."indicator_id";
SELECT setval('"p_rsf"."indicators_indicator_id_seq"', 157884, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."label_ids_label_id_seq"
OWNED BY "p_rsf"."label_ids"."label_id";
SELECT setval('"p_rsf"."label_ids_label_id_seq"', 2734, true);

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
OWNED BY "p_rsf"."dashboard_reports"."report_id";
SELECT setval('"p_rsf"."reports_report_id_seq"', 76, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_data_calculation_profiles_calculation_profile_id_seq"', 5170327, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_data_checks_evaluation_id_seq"', 4553830, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_data_cohort_sequence"', 121405, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_data_data_id_seq"', 31673565, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
SELECT setval('"p_rsf"."rsf_pfcbl_ids_rsf_pfcbl_id_seq"', 691744, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."rsf_program_facility_template_headers_header_id_seq"
OWNED BY "p_rsf"."rsf_setup_template_headers"."header_id";
SELECT setval('"p_rsf"."rsf_program_facility_template_headers_header_id_seq"', 1264, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."rsf_settings_archive_archive_id_seq"
OWNED BY "p_rsf"."rsf_setup_archive"."archive_id";
SELECT setval('"p_rsf"."rsf_settings_archive_archive_id_seq"', 1718, true);

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "p_rsf"."rsf_setup_checks_config_config_id_seq"
OWNED BY "p_rsf"."rsf_setup_checks_config"."config_id";
SELECT setval('"p_rsf"."rsf_setup_checks_config_config_id_seq"', 80, true);

-- ----------------------------
-- Indexes structure for table !dep-indicator_check_guidance
-- ----------------------------
CREATE INDEX "indicator_check_guidance_indicator_check_id_for_indicator_i_idx" ON "p_rsf"."!dep-indicator_check_guidance" USING btree (
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "for_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "indicator_check_guidance_unique_guidance_idx" ON "p_rsf"."!dep-indicator_check_guidance" USING btree (
  "for_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  normalizelabel(guidance) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table !dep-indicator_check_guidance
-- ----------------------------
CREATE TRIGGER "trigger_guidance_global_subscription" AFTER INSERT ON "p_rsf"."!dep-indicator_check_guidance"
FOR EACH ROW
WHEN ((new.for_pfcbl_category = 'global'::text))
EXECUTE PROCEDURE "p_rsf"."global_guidance_subscription"();

-- ----------------------------
-- Checks structure for table !dep-indicator_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-indicator_check_guidance" ADD CONSTRAINT "variance_is_zero_or_not_fraction_less_than_1" CHECK (variance_threshold = 0::numeric OR abs(variance_threshold) >= 1::numeric);
ALTER TABLE "p_rsf"."!dep-indicator_check_guidance" ADD CONSTRAINT "for_pfcbl_category_is_program_or_facility" CHECK (for_pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text]));
ALTER TABLE "p_rsf"."!dep-indicator_check_guidance" ADD CONSTRAINT "resolve_or_ignore_not_both" CHECK ((is_resolving_guidance AND is_ignoring_guidance) = false);
COMMENT ON CONSTRAINT "variance_is_zero_or_not_fraction_less_than_1" ON "p_rsf"."!dep-indicator_check_guidance" IS 'To ensure 15% is entered as 15 instead of 0.15';

-- ----------------------------
-- Primary Key structure for table !dep-indicator_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-indicator_check_guidance" ADD CONSTRAINT "indicator_check_guidance_pkey" PRIMARY KEY ("indicator_check_guidance_id");

-- ----------------------------
-- Primary Key structure for table !dep-reporting_cohort_info
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-reporting_cohort_info" ADD CONSTRAINT "reporting_cohort_uploads_pkey" PRIMARY KEY ("reporting_cohort_id");

-- ----------------------------
-- Indexes structure for table !dep-rsf_pfcbl_reporting
-- ----------------------------
CREATE UNIQUE INDEX "rsf_pfcbl_reporting-entity_indicator_date-udx" ON "p_rsf"."!dep-rsf_pfcbl_reporting" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_reporting_created_by_data_id_idx" ON "p_rsf"."!dep-rsf_pfcbl_reporting" USING btree (
  "created_by_data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_reporting_rsf_pfcbl_id_idx" ON "p_rsf"."!dep-rsf_pfcbl_reporting" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table !dep-rsf_pfcbl_reporting
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_pfcbl_reporting" ADD CONSTRAINT "rsf_pfcbl_reporting_pkey" PRIMARY KEY ("rsf_pfcbl_id", "reporting_asof_date");

-- ----------------------------
-- Indexes structure for table !dep-rsf_program_facility_check_guidance
-- ----------------------------
CREATE UNIQUE INDEX "rsf_program_facility_check_gu_indicator_check_guidance_id_r_idx" ON "p_rsf"."!dep-rsf_program_facility_check_guidance" USING btree (
  "indicator_check_guidance_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "rsf_facility_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table !dep-rsf_program_facility_check_guidance
-- ----------------------------
CREATE TRIGGER "trigger_check_valid_guidance_entry" BEFORE INSERT ON "p_rsf"."!dep-rsf_program_facility_check_guidance"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."check_valid_guidance_entry"();

-- ----------------------------
-- Primary Key structure for table !dep-rsf_program_facility_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_program_facility_check_guidance" ADD CONSTRAINT "rsf_program_check_guidance_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_check_guidance_id");

-- ----------------------------
-- Primary Key structure for table !dep-rsf_program_settings
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_program_settings" ADD CONSTRAINT "rsf_program_settings_pkey" PRIMARY KEY ("rsf_program_id", "setting_name");

-- ----------------------------
-- Indexes structure for table dashboard_exports
-- ----------------------------
CREATE UNIQUE INDEX "exporting_cohorts_reporting_key_idx" ON "p_rsf"."dashboard_exports" USING btree (
  "reporting_key" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table dashboard_exports
-- ----------------------------
CREATE TRIGGER "trigger_set_exporting_cohorts_reporting_key" BEFORE INSERT OR UPDATE ON "p_rsf"."dashboard_exports"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."set_exporting_cohorts_reporting_key"();

-- ----------------------------
-- Primary Key structure for table dashboard_exports
-- ----------------------------
ALTER TABLE "p_rsf"."dashboard_exports" ADD CONSTRAINT "exporting_cohorts_pkey" PRIMARY KEY ("exporting_cohort_id");

-- ----------------------------
-- Primary Key structure for table dashboard_reports
-- ----------------------------
ALTER TABLE "p_rsf"."dashboard_reports" ADD CONSTRAINT "reports_pkey" PRIMARY KEY ("report_id");

-- ----------------------------
-- Primary Key structure for table export_template_reports
-- ----------------------------
ALTER TABLE "p_rsf"."export_template_reports" ADD CONSTRAINT "export_template_reports_pkey" PRIMARY KEY ("export_template_report_id");

-- ----------------------------
-- Primary Key structure for table export_templates
-- ----------------------------
ALTER TABLE "p_rsf"."export_templates" ADD CONSTRAINT "export_templates_pkey" PRIMARY KEY ("export_template_id");

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
  "computation_group" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "unique_check_formula_title_udx" ON "p_rsf"."indicator_check_formulas" USING btree (
  normalizelabel(check_formula_title) COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table indicator_check_formulas
-- ----------------------------
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
ALTER TABLE "p_rsf"."indicator_checks" ADD CONSTRAINT "calculator_checks_are_system_checks" CHECK (
CASE
    WHEN is_calculator_check = true THEN is_system = true
    ELSE true
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
-- Primary Key structure for table indicator_classifications
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_classifications" ADD CONSTRAINT "indicator_classifications_pkey" PRIMARY KEY ("classification_id");

-- ----------------------------
-- Primary Key structure for table indicator_data_types
-- ----------------------------
ALTER TABLE "p_rsf"."indicator_data_types" ADD CONSTRAINT "indicator_data_types_pkey" PRIMARY KEY ("data_type");

-- ----------------------------
-- Indexes structure for table indicator_formula_parameters
-- ----------------------------
CREATE INDEX "calculation_parameters-formula_rank_indicator-idx" ON "p_rsf"."indicator_formula_parameters" USING btree (
  "formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "parameter_pfcbl_rank" "pg_catalog"."int2_ops" ASC NULLS LAST,
  "parameter_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "ifp-formula_id-idx" ON "p_rsf"."indicator_formula_parameters" USING btree (
  "formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
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
CREATE TRIGGER "trigger_set_indicator_formula_ids" BEFORE INSERT OR UPDATE OF "indicator_id", "formula", "formula_sort", "formula_grouping_pfcbl_rank", "modification_time" ON "p_rsf"."indicator_formulas"
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
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "disallowed_calculation_parameter_issuances" CHECK ((formula ~ '\.issuances'::text) = false);
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "valid_overwrite_type" CHECK (overwrite::text = ANY (ARRAY['allow'::character varying::text, 'deny'::character varying::text, 'missing'::character varying::text, 'unchanged'::character varying::text]));
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "dot_all_parameters_use_timeseries_values" CHECK (
CASE
    WHEN formula ~ '\.all'::text THEN formula ~ 'timeseries'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicator_formulas" ADD CONSTRAINT "system_use_delimiters_not_allowed_in_formula_titles" CHECK (NOT formula_title ~ '[#{}]'::text);
COMMENT ON CONSTRAINT "disallowed_calculation_parameter_issuances" ON "p_rsf"."indicator_formulas" IS 'calculations can see all issuances and also the issuance group ID as needed for disaggregation, but are not expected to calculate values based on which issuances are within a series (or not); this is meaningful for checks';
COMMENT ON CONSTRAINT "dot_all_parameters_use_timeseries_values" ON "p_rsf"."indicator_formulas" IS 'The .all parameter will return an embedded data.table object as a list within each row: the columns of the data.table are "timeseries", "timeseries.unit", "timeseries.reporteddate", "timeseries.changed", "timeseries.updated", "timeseries.reportnumber" and it is essential therefore that anyone using a .all parameter in a formula also uses a .timeseries value, since thats where the actual data is represented (and not using it means there is a formula error)';
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
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "system_indicators_are_default_subscriable" CHECK (
CASE
    WHEN is_system = true THEN default_subscription IS TRUE
    ELSE true
END);
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
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "list_keyword_allows_multiples" CHECK (
CASE
    WHEN indicator_name::text ~* 'list'::text THEN indicator_options_group_allows_multiples IS TRUE
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "check_valid_indicator_name" CHECK (indicator_name::text ~* '^[a-zA-Z0-9_]+$'::text);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "check_indicator_name_includes_data_category" CHECK (indicator_name::text ~* (('^(sys_)?'::text || data_category::text) || '_.*$'::text) OR indicator_name::text ~* '^rsf_new_indicator.*$'::text);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "unit_fx_indicator_exists" CHECK (
CASE
    WHEN data_type::text <> 'currency'::text THEN true
    WHEN data_unit::text = 'LCU'::text THEN unit_fx_indicator_id IS NULL
    WHEN unit_fx_indicator_id IS NOT NULL THEN data_unit::text <> 'LCU'::text
    ELSE false
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "check_options_group_allows_multiples_can_only_be_text" CHECK (COALESCE(indicator_options_group_allows_multiples, false) = true AND data_type::text = 'text'::text OR COALESCE(indicator_options_group_allows_multiples, false) = false);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_data_type_units_format" CHECK (
CASE
    WHEN data_type::text = 'currency'::text THEN data_unit IS NOT NULL AND data_unit::text ~ '^[A-Z]{3}$'::text
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_has_data_unit" CHECK (data_category::text = 'currency'::text AND data_unit IS NOT NULL OR NOT data_category::text = 'currency'::text);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_data_type_is_calculated" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text THEN is_calculated
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
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "unit_fx_is_valid" CHECK (unit_fx_method IS NULL OR (unit_fx_method = ANY (ARRAY['calculation'::text, 'parameter'::text, 'fx'::text])));
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "unit_fx_source_is_valid" CHECK (unit_fx_source IS NULL OR (unit_fx_source = ANY (ARRAY['default'::text, 'global'::text])));
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_allowed_data_categories" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text THEN data_category::text = ANY (ARRAY['global'::character varying::text, 'facility'::character varying::text])
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "unit_fx_indicator_id_cannot_be_self" CHECK (indicator_id IS DISTINCT FROM unit_fx_indicator_id);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_data_type_ratio_alphabetic_order" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text THEN p_rsf.fx_currency_ratio_has_alphabetic_order(data_unit::text)
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "currency_ratio_LCU_disallowed" CHECK (
CASE
    WHEN data_type::text = 'currency_ratio'::text THEN (data_unit::text ~* 'LCU'::text) = false AND (data_unit::text ~* 'LCY'::text) = false
    ELSE true
END);
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicator_names_start_with_lower_case_letters" CHECK ((indicator_name::text ~ '^[A-Z]'::text) = false);
COMMENT ON CONSTRAINT "is_data_unit_has_valid_sys_category" ON "p_rsf"."indicators" IS 'Presently, only currencies can be defined by is_data_unit (perhaps this will change in the future, and would require updates in template_parse_formats)';
COMMENT ON CONSTRAINT "check_valid_indicator_name" ON "p_rsf"."indicators" IS 'indicator_name meets syntax';
COMMENT ON CONSTRAINT "check_indicator_name_includes_data_category" ON "p_rsf"."indicators" IS 'Useful for human interpretation of the indicator; but also necessary for system to resolve the indicator''s data category from the name itself.';
COMMENT ON CONSTRAINT "check_options_group_allows_multiples_can_only_be_text" ON "p_rsf"."indicators" IS 'Multiple selections stored as &-delimited text, so options group that are numerics, etc will throw errors within the application when checking/enforcing that data is appropriately typed.  Meanwhile, fully checking all potential data being a multi-select and type introduces extreme complexity.  Text-only multiples is a fair compromise and only indicator level calculations or checks need to worry about parsing concatenated values';
COMMENT ON CONSTRAINT "currency_has_data_unit" ON "p_rsf"."indicators" IS 'Currency must specify a currency unit (LCU if not currency specific)';
COMMENT ON CONSTRAINT "currency_ratio_data_type_is_calculated" ON "p_rsf"."indicators" IS 'Currecy ratio data types must be calcualted to ensure that the currency ratio is (re)validated each reporting period.  Pegged currencies will have a fixed fx rate to the currency they are pegged to and will not change over time; whereas floating currencies will almost certainly change from quarter to quarter.  So enforcing a calculation will ensure this is checked and also that a flag will be raised if a user forgets to update an fx rate that should be updated.';
COMMENT ON CONSTRAINT "currency_ratio_data_type_units_format" ON "p_rsf"."indicators" IS 'Entity currency unit must define a currency unit value';
COMMENT ON CONSTRAINT "is_data_unit_is_text" ON "p_rsf"."indicators" IS 'Data units must be text data types.';
COMMENT ON CONSTRAINT "currency_ratio_allowed_data_categories" ON "p_rsf"."indicators" IS 'Currency ratios (ie, fx rates) can only be defined at global (which is IFC official system rates); or at facility levels -- rate sources defined by and provided by the respective facilities per contractual agreement';
COMMENT ON CONSTRAINT "currency_ratio_data_type_ratio_alphabetic_order" ON "p_rsf"."indicators" IS 'Currency ratios are entered in alphabetic order, unless LCU is used in the denominator';
COMMENT ON CONSTRAINT "indicator_names_start_with_lower_case_letters" ON "p_rsf"."indicators" IS 'System dashboard uses upper case letters to identify system columns';

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
-- Indexes structure for table reporting_cohorts
-- ----------------------------
CREATE INDEX "reporting_cohorts-id_date-idx" ON "p_rsf"."reporting_cohorts" USING btree (
  "reporting_rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "reporting_cohorts-reporting_time-udx" ON "p_rsf"."reporting_cohorts" USING btree (
  "reporting_time" "pg_catalog"."timestamptz_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "reporting_cohorts_reporting_cohort_id_reporting_asof_date_fkidx" ON "p_rsf"."reporting_cohorts" USING btree (
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST
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
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "reporting_cohorts_reporting_cohort_id_is_reported_cohort_key" UNIQUE ("reporting_cohort_id", "is_reported_cohort");
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "unique_reporting_time" UNIQUE ("reporting_time");
COMMENT ON CONSTRAINT "unique_reporting_time" ON "p_rsf"."reporting_cohorts" IS 'To ensure no duplicated reporting times';

-- ----------------------------
-- Checks structure for table reporting_cohorts
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "cannot_be_calculated_and_reported" CHECK (
CASE
    WHEN is_reported_cohort = true THEN is_calculated_cohort = false
    ELSE true
END);
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "data_asof_date_same_quarter_as_reporting_asof_date" CHECK (data_asof_date >= date_trunc('quarter'::text, reporting_asof_date::timestamp with time zone)::date AND data_asof_date <= reporting_asof_date);

-- ----------------------------
-- Primary Key structure for table reporting_cohorts
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_cohorts" ADD CONSTRAINT "rsf_reporting_cohorts_pkey" PRIMARY KEY ("reporting_cohort_id");

-- ----------------------------
-- Primary Key structure for table reporting_dates
-- ----------------------------
ALTER TABLE "p_rsf"."reporting_dates" ADD CONSTRAINT "reporting_dates_pkey" PRIMARY KEY ("quarter_end_date");

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
-- Triggers structure for table reporting_imports
-- ----------------------------
CREATE TRIGGER "trigger_reporting_imports_0_insert_set_sequence_name" BEFORE INSERT OR UPDATE ON "p_rsf"."reporting_imports"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."reporting_imports_set_sequence_name"();

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
CREATE INDEX "rsf_data-cohort_indicator-idx" ON "p_rsf"."rsf_data" USING btree (
  "reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data-id_indicator_date-idx" ON "p_rsf"."rsf_data" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" DESC NULLS FIRST
);
CREATE INDEX "rsf_data-indicator_id-fkidx" ON "p_rsf"."rsf_data" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data-reporting_cohort_id-fkidx" ON "p_rsf"."rsf_data" USING btree (
  "reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data-rsf_pfcbl_id&indicator_id_idx" ON "p_rsf"."rsf_data" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data-rsf_pfcbl_id-fkidx" ON "p_rsf"."rsf_data" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."rsf_data" CLUSTER ON "rsf_data-rsf_pfcbl_id-fkidx";

-- ----------------------------
-- Triggers structure for table rsf_data
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_0_inserted_data_unit_lcu" BEFORE INSERT ON "p_rsf"."rsf_data"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_inserted_data_unit_lcu"();
CREATE TRIGGER "trigger_rsf_data_1_inserted_data_integrity" AFTER INSERT ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_inserted_data_integrity"();
CREATE TRIGGER "trigger_rsf_data_3_modified_deleted_data_current" AFTER DELETE ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_data_current"();
CREATE TRIGGER "trigger_rsf_data_3_modified_inserted_data_current" AFTER INSERT ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_data_current"();
CREATE TRIGGER "trigger_rsf_data_3_modified_updated_data_current" AFTER UPDATE ON "p_rsf"."rsf_data"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_modified_data_current"();
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
ALTER TABLE "p_rsf"."rsf_data" CLUSTER ON "rsf_data-rsf_pfcbl_id-fkidx";

-- ----------------------------
-- Indexes structure for table rsf_data_calculation_evaluations
-- ----------------------------
CREATE INDEX "calculation_evaluations-pf_id_date_rank-idx" ON "p_rsf"."rsf_data_calculation_evaluations" USING btree (
  "rsf_pf_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "calculation_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "formula_calculation_rank" "pg_catalog"."int2_ops" ASC NULLS LAST
);
CREATE INDEX "calculation_evaluations-pf_id_indicator-fkidx" ON "p_rsf"."rsf_data_calculation_evaluations" USING btree (
  "rsf_pf_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
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
CREATE TRIGGER "trigger_calculation_evaluation_1_deleted_do_validation" BEFORE DELETE ON "p_rsf"."rsf_data_calculation_evaluations"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_calculation_evaluation_validation"();
CREATE TRIGGER "trigger_calculation_evaluation_1_inserted_reset_futures" AFTER INSERT ON "p_rsf"."rsf_data_calculation_evaluations"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_calculation_evaluation_revalidate"();
CREATE TRIGGER "trigger_rsf_data_calculation_evaluation_error_check" AFTER INSERT ON "p_rsf"."rsf_data_calculation_evaluations"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_calculation_evaluation_error_check"();

-- ----------------------------
-- Primary Key structure for table rsf_data_calculation_evaluations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_calculation_evaluations" ADD CONSTRAINT "rsf_data_calculation_evaluations_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_id", "calculation_asof_date");

-- ----------------------------
-- Cluster option for table rsf_data_calculation_evaluations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_calculation_evaluations" CLUSTER ON "calculation_evaluations-rsf_pfcbl_id_idx";

-- ----------------------------
-- Indexes structure for table rsf_data_calculation_validations
-- ----------------------------
CREATE INDEX "calculation_validations-entity_indicator_date-idx" ON "p_rsf"."rsf_data_calculation_validations" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "calculation_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "calculation_validations_entity_indicator_date_time_idx" ON "p_rsf"."rsf_data_calculation_validations" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "calculation_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "validation_time" "pg_catalog"."timestamptz_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_calculation_validations-data_id-fkidx" ON "p_rsf"."rsf_data_calculation_validations" USING btree (
  "data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_calculation_validations_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_data_calculation_validations" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

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
-- Triggers structure for table rsf_data_check_evaluations
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_check_evaluation_allow" BEFORE INSERT ON "p_rsf"."rsf_data_check_evaluations"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_check_evaluation_allowed"();

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
CREATE INDEX "rsf_data_checks-check_formula_id-fkidx" ON "p_rsf"."rsf_data_checks" USING btree (
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-current-data_id" ON "p_rsf"."rsf_data_checks" USING btree (
  "data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE check_data_id_is_current = true;
CREATE INDEX "rsf_data_checks-data_id&check_asof_date_ifx" ON "p_rsf"."rsf_data_checks" USING btree (
  "data_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-data_id-fkidx" ON "p_rsf"."rsf_data_checks" USING btree (
  "data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "rsf_data_checks-formula_application_unique_per_entity_date-udx" ON "p_rsf"."rsf_data_checks" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE check_data_id_is_current = true AND check_formula_id IS NOT NULL;
COMMENT ON INDEX "p_rsf"."rsf_data_checks-formula_application_unique_per_entity_date-udx" IS 'For non-null formula_ids (ie, user checks, not system checks where formula_id will be NULL), then ensure each rsf_pfcbl_id+check_asof_date+check_formula_id is uniquely applied to ensure that data updates do not cause the same check to be applied on multiple different indicators ';
CREATE INDEX "rsf_data_checks-import_id-fkidx" ON "p_rsf"."rsf_data_checks" USING btree (
  "for_import_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-indicator_check_id-fkidx" ON "p_rsf"."rsf_data_checks" USING btree (
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-pfcbl&indicator&check_asof_date_idx" ON "p_rsf"."rsf_data_checks" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-pfcbl&indicator_idx" ON "p_rsf"."rsf_data_checks" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks-rsf_pfcbl_id-fkidx" ON "p_rsf"."rsf_data_checks" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_data_checks
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_checks_0_restore_archive" BEFORE INSERT ON "p_rsf"."rsf_data_checks"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_0_restore_archive"();
CREATE TRIGGER "trigger_rsf_data_checks_1_modified_flagged" AFTER INSERT OR UPDATE OF "data_sys_flags" ON "p_rsf"."rsf_data_checks"
FOR EACH ROW
WHEN ((new.data_sys_flags IS NOT NULL))
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_flagged_data_cascade"();
CREATE TRIGGER "trigger_rsf_data_checks_2_modified_validate_permissions" AFTER UPDATE OF "check_status" ON "p_rsf"."rsf_data_checks"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_validate_permissions"();
CREATE TRIGGER "trigger_rsf_data_checks_3_modified_cleanup" AFTER INSERT OR DELETE ON "p_rsf"."rsf_data_checks"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_checks_clean_archive"();

-- ----------------------------
-- Checks structure for table rsf_data_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "check_resolved_requires_comment" CHECK (
CASE
    WHEN check_status = 'resolved'::text THEN check_status_comment IS NOT NULL AND check_status_user_id IS NOT NULL
    ELSE check_status = 'active'::text
END);
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "check_status_active_or_resolved" CHECK (check_status = ANY (ARRAY['active'::text, 'resolved'::text]));

-- ----------------------------
-- Primary Key structure for table rsf_data_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_evaluation_id_pkey" PRIMARY KEY ("evaluation_id");

-- ----------------------------
-- Indexes structure for table rsf_data_checks_archive
-- ----------------------------
CREATE INDEX "rsf_data_checks_archive-archive_time-idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "archive_time" "pg_catalog"."timestamptz_ops" DESC NULLS LAST
);
CREATE INDEX "rsf_data_checks_archive-check_formula_id-idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks_archive-indicator_check_id-idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks_archive-indicator_id-idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks_archive-rsf_pfcbl_id-idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks_archive-sys_name-idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "sys_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_checks_archive-sys_name_indicator_date_check-idx" ON "p_rsf"."rsf_data_checks_archive" USING btree (
  "sys_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST,
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

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
CREATE INDEX "rsf_data_current-data_time-idx" ON "p_rsf"."rsf_data_current" USING btree (
  "data_time" "pg_catalog"."timestamptz_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current-data_unit_data_id_idx" ON "p_rsf"."rsf_data_current" USING btree (
  "data_unit_data_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current-entity_indicator_date-idx" ON "p_rsf"."rsf_data_current" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_data_current-rsf_pfcbl_id_indicator_id_idx" ON "p_rsf"."rsf_data_current" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
ALTER TABLE "p_rsf"."rsf_data_current" CLUSTER ON "rsf_data_current-rsf_pfcbl_id_indicator_id_idx";
CREATE INDEX "rsf_data_current-time_indicator_id-idx" ON "p_rsf"."rsf_data_current" USING btree (
  "data_time" "pg_catalog"."timestamptz_ops" DESC NULLS FIRST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_data_current
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_current_0_modified_unchanged" BEFORE INSERT OR UPDATE ON "p_rsf"."rsf_data_current"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_modified_unchanged"();
CREATE TRIGGER "trigger_rsf_data_current_4_inserted_checks_set_current" AFTER DELETE ON "p_rsf"."rsf_data_current"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_checks_set_current"();
CREATE TRIGGER "trigger_rsf_data_current_4_updated_checks_set_current" AFTER UPDATE ON "p_rsf"."rsf_data_current"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_checks_set_current"();

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
-- Triggers structure for table rsf_data_current_fx
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_current_fx_1_modified_fx_deleted" AFTER DELETE ON "p_rsf"."rsf_data_current_fx"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_fx_modified"();
CREATE TRIGGER "trigger_rsf_data_current_fx_1_modified_fx_updated" AFTER UPDATE ON "p_rsf"."rsf_data_current_fx"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_fx_modified"();

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
CREATE INDEX "rsf_data_current_names_and_ids_sys_name_gin" ON "p_rsf"."rsf_data_current_names_and_ids" USING gin (
  "sys_name" COLLATE "pg_catalog"."default" "p_rsf"."gin_trgm_ops"
);
CREATE UNIQUE INDEX "rsf_data_current_names_and_ids_sys_name_udx" ON "p_rsf"."rsf_data_current_names_and_ids" USING btree (
  "sys_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
COMMENT ON INDEX "p_rsf"."rsf_data_current_names_and_ids_sys_name_udx" IS 'Clients will sometimes make typos in client names, then correct them later, creating timeseries repetitions in names';

-- ----------------------------
-- Triggers structure for table rsf_data_current_names_and_ids
-- ----------------------------
CREATE TRIGGER "trigger_rsf_data_current_nids_1_inserted_set_sysname" AFTER INSERT ON "p_rsf"."rsf_data_current_names_and_ids"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_nids_set_sysname"();
CREATE TRIGGER "trigger_rsf_data_current_nids_1_updated_set_sysname" AFTER UPDATE ON "p_rsf"."rsf_data_current_names_and_ids"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_nids_set_sysname"();
CREATE TRIGGER "trigger_rsf_data_current_nids_2_modified_restoring" AFTER UPDATE OF "sys_name" ON "p_rsf"."rsf_data_current_names_and_ids"
FOR EACH ROW
WHEN ((new.pfcbl_category = ANY (ARRAY['global'::text, 'program'::text, 'facility'::text])))
EXECUTE PROCEDURE "p_rsf"."rsf_data_current_names_and_ids_restoring"();

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
-- Checks structure for table rsf_data_sys_flags
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_sys_flags" ADD CONSTRAINT "value_is_a_bit" CHECK (data_flag_value > 0 AND (data_flag_value::integer & (data_flag_value - 1)) = 0);

-- ----------------------------
-- Primary Key structure for table rsf_data_sys_flags
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_sys_flags" ADD CONSTRAINT "rsf_data_sys_flags_pkey" PRIMARY KEY ("data_flag_value");

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
CREATE INDEX "rsf_pfcbl_ids-globalfamily-idx" ON "p_rsf"."rsf_pfcbl_ids" USING gin (
  "rsf_gpfcbl_family" "pg_catalog"."array_ops"
);
CREATE INDEX "rsf_pfcbl_ids-id_creation_date-idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "created_in_reporting_asof_date" "pg_catalog"."date_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids-rsf_pf_id-idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_pf_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids-rsf_pf_id_pfcbl_rank-idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_pf_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "pfcbl_category_rank" "pg_catalog"."int2_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_created_by_reporting_cohort_id_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "created_by_reporting_cohort_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_pfcbl_ids_desc_by_borrower_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_borrower_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "pfcbl_category_rank" "pg_catalog"."int2_ops" ASC NULLS LAST,
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE pfcbl_category_rank > 4 AND rsf_borrower_id IS NOT NULL;
CREATE INDEX "rsf_pfcbl_ids_desc_by_client_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_client_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "pfcbl_category_rank" "pg_catalog"."int2_ops" ASC NULLS LAST,
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE pfcbl_category_rank > 3 AND rsf_client_id IS NOT NULL;
CREATE INDEX "rsf_pfcbl_ids_desc_by_facility_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_facility_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "pfcbl_category_rank" "pg_catalog"."int2_ops" ASC NULLS LAST,
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE pfcbl_category_rank > 2 AND rsf_facility_id IS NOT NULL;
CREATE INDEX "rsf_pfcbl_ids_desc_by_program_idx" ON "p_rsf"."rsf_pfcbl_ids" USING btree (
  "rsf_program_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "pfcbl_category_rank" "pg_catalog"."int2_ops" ASC NULLS LAST,
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE pfcbl_category_rank > 1;
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
CREATE TRIGGER "trigger_insert_rsf_pfcbl_id_4_initialize_evaluations" AFTER INSERT ON "p_rsf"."rsf_pfcbl_ids"
FOR EACH STATEMENT
EXECUTE PROCEDURE "p_rsf"."insert_rsf_pfcbl_id_evaluations"();
CREATE TRIGGER "trigger_rsf_pfcbl_id_0_deleted_archive" BEFORE DELETE ON "p_rsf"."rsf_pfcbl_ids"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_pfcbl_id_deleted_archive"();
CREATE TRIGGER "trigger_rsf_pfcbl_id_0_insert_family_ids" BEFORE INSERT ON "p_rsf"."rsf_pfcbl_ids"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_pfcbl_id_insert_family_ids"();

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
-- Checks structure for table rsf_programs
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_programs" ADD CONSTRAINT "programs_can_report_quarterly_or_monthly" CHECK (ARRAY[reporting_period::text] && ARRAY['quarter'::text]);
COMMENT ON CONSTRAINT "programs_can_report_quarterly_or_monthly" ON "p_rsf"."rsf_programs" IS 'In theory, different programs might have monthly, etc reporting obligations and so this was designed to accommodate.  In practice, RSFs report quarterly and this assumption is baked-in so much that this field isn''t super meaningful.';

-- ----------------------------
-- Primary Key structure for table rsf_programs
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_programs" ADD CONSTRAINT "rsf_programs_pkey" PRIMARY KEY ("rsf_program_id");

-- ----------------------------
-- Indexes structure for table rsf_setup_archive
-- ----------------------------
CREATE INDEX "rsf_setup_archive_sys_name_idx" ON "p_rsf"."rsf_setup_archive" USING btree (
  "sys_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "rsf_setup_archive_sys_name_settings_source_idx" ON "p_rsf"."rsf_setup_archive" USING btree (
  "sys_name" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST,
  "settings_source" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);

-- ----------------------------
-- Primary Key structure for table rsf_setup_archive
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_archive" ADD CONSTRAINT "rsf_settings_archive_pkey" PRIMARY KEY ("archive_id");

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
-- Checks structure for table rsf_setup_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "check_has_no_cohort_fk" CHECK (
CASE
    WHEN is_auto_subscribed IS FALSE THEN auto_subscribed_by_reporting_cohort_id IS NULL AND comments_user_id IS NOT NULL
    ELSE true
END);

-- ----------------------------
-- Primary Key structure for table rsf_setup_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "rsf_setup_checks_pkey" PRIMARY KEY ("rsf_pfcbl_id", "check_formula_id");

-- ----------------------------
-- Indexes structure for table rsf_setup_checks_config
-- ----------------------------
CREATE UNIQUE INDEX "rsf_setup_checks_config_rsf_pfcbl_id_for_indicator_id_indic_idx" ON "p_rsf"."rsf_setup_checks_config" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "for_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "check_formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
COMMENT ON INDEX "p_rsf"."rsf_setup_checks_config_rsf_pfcbl_id_for_indicator_id_indic_idx" IS 'with option NULLS NOT DISTINCT;';
CREATE INDEX "rsf_setup_checks_config_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_setup_checks_config" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "rsf_setup_checks_config_uids_udx" ON "p_rsf"."rsf_setup_checks_config" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "for_indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_check_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  COALESCE(check_formula_id, '-1'::integer) "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Uniques structure for table rsf_setup_checks_config
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config_uids_ucnst" UNIQUE ("rsf_pfcbl_id", "for_indicator_id", "indicator_check_id", "check_formula_id");

-- ----------------------------
-- Checks structure for table rsf_setup_checks_config
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "validate_config_check_class" CHECK (config_check_class = ANY (ARRAY['info'::text, 'warning'::text, 'error'::text, 'critical'::text]));

-- ----------------------------
-- Primary Key structure for table rsf_setup_checks_config
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config_pkey" PRIMARY KEY ("config_id");

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
CREATE INDEX "rsf_setup_indicators-rsf_pfcbl_id_formula_id" ON "p_rsf"."rsf_setup_indicators" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "formula_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE UNIQUE INDEX "rsf_setup_indicators-rsf_pfcbl_id_indicator_id" ON "p_rsf"."rsf_setup_indicators" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "indicator_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_setup_indicators-subscribed_pfcbl_id-idx" ON "p_rsf"."rsf_setup_indicators" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
) WHERE is_subscribed IS TRUE;

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
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "subscribed_have_no_cohort_fk" CHECK (
CASE
    WHEN is_auto_subscribed IS FALSE THEN auto_subscribed_by_reporting_cohort_id IS NULL AND comments_user_id IS NOT NULL
    ELSE true
END);

-- ----------------------------
-- Primary Key structure for table rsf_setup_indicators
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_pkey" PRIMARY KEY ("rsf_pfcbl_id", "indicator_id");

-- ----------------------------
-- Indexes structure for table rsf_setup_template_headers
-- ----------------------------
CREATE INDEX "rsf_setup_template_headers_rsf_pfcbl_id_idx" ON "p_rsf"."rsf_setup_template_headers" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_setup_template_headers_rsf_pfcbl_id_template_id_temp_idx" ON "p_rsf"."rsf_setup_template_headers" USING btree (
  "rsf_pfcbl_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "template_id" "pg_catalog"."int4_ops" ASC NULLS LAST,
  "template_header_full_normalized" COLLATE "pg_catalog"."default" "pg_catalog"."text_ops" ASC NULLS LAST
);
CREATE INDEX "rsf_setup_template_headers_template_id_idx" ON "p_rsf"."rsf_setup_template_headers" USING btree (
  "template_id" "pg_catalog"."int4_ops" ASC NULLS LAST
);

-- ----------------------------
-- Triggers structure for table rsf_setup_template_headers
-- ----------------------------
CREATE TRIGGER "trigger_changed_rsf_setup_template_headers" BEFORE INSERT OR UPDATE ON "p_rsf"."rsf_setup_template_headers"
FOR EACH ROW
EXECUTE PROCEDURE "p_rsf"."rsf_setup_template_headers_normalized"();

-- ----------------------------
-- Uniques structure for table rsf_setup_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "unique_entity_template_header_action_mapping" UNIQUE ("rsf_pfcbl_id", "template_id", "template_header_full_normalized", "action_mapping");

-- ----------------------------
-- Checks structure for table rsf_setup_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "no_regexp_star" CHECK ((template_header ~ '\*'::text) = false);
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "valid_actions" CHECK (action = ANY (ARRAY['default'::text, 'ignore'::text, 'remap'::text, 'unmap'::text, 'check'::text, 'calculate'::text, 'parse'::text, 'section'::text]));
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "valid_mappings" CHECK (true OR
CASE
    WHEN action = ANY (ARRAY['default'::text, 'ignore'::text, 'parse'::text, 'section'::text]) THEN map_indicator_id IS NULL AND map_formula_id IS NULL AND map_check_formula_id IS NULL
    WHEN action = ANY (ARRAY['remap'::text, 'unmap'::text]) THEN map_indicator_id IS NOT NULL AND map_formula_id IS NULL AND map_check_formula_id IS NULL
    WHEN action = 'calculate'::text THEN map_indicator_id IS NULL AND map_formula_id IS NOT NULL AND map_check_formula_id IS NULL
    WHEN action = 'check'::text THEN map_indicator_id IS NULL AND map_formula_id IS NULL AND map_check_formula_id IS NOT NULL
    ELSE true
END);
COMMENT ON CONSTRAINT "valid_actions" ON "p_rsf"."rsf_setup_template_headers" IS 'Will allow duplicates when mappings are all NULL';
COMMENT ON CONSTRAINT "valid_mappings" ON "p_rsf"."rsf_setup_template_headers" IS 'Not used because UI requires drop down select and then saves selection before mapping selection can be made';

-- ----------------------------
-- Primary Key structure for table rsf_setup_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "rsf_program_facility_templates_pkey" PRIMARY KEY ("header_id");

-- ----------------------------
-- Foreign Keys structure for table !dep-indicator_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-indicator_check_guidance" ADD CONSTRAINT "indicator_check_guidance_for_indicator_id_fkey" FOREIGN KEY ("for_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."!dep-indicator_check_guidance" ADD CONSTRAINT "indicator_check_guidance_indicator_check_id_fkey" FOREIGN KEY ("indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table !dep-reporting_cohort_info
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-reporting_cohort_info" ADD CONSTRAINT "reporting_cohort_info-reporting_cohort_id_fkey" FOREIGN KEY ("reporting_cohort_id") REFERENCES "p_rsf"."reporting_cohorts" ("reporting_cohort_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table !dep-rsf_pfcbl_reporting
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_pfcbl_reporting" ADD CONSTRAINT "rsf_pfcbl_reporting-creatd_by_data_id_fkey" FOREIGN KEY ("created_by_data_id") REFERENCES "p_rsf"."rsf_data" ("data_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."!dep-rsf_pfcbl_reporting" ADD CONSTRAINT "rsf_pfcbl_reporting-rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table !dep-rsf_program_facility_check_guidance
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_program_facility_check_guidance" ADD CONSTRAINT "rsf_program_facility_check_gui_indicator_check_guidance_id_fkey" FOREIGN KEY ("indicator_check_guidance_id") REFERENCES "p_rsf"."!dep-indicator_check_guidance" ("indicator_check_guidance_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."!dep-rsf_program_facility_check_guidance" ADD CONSTRAINT "rsf_program_facility_check_guidance_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table !dep-rsf_program_settings
-- ----------------------------
ALTER TABLE "p_rsf"."!dep-rsf_program_settings" ADD CONSTRAINT "rsf_program_settings_rsf_program_id_fkey" FOREIGN KEY ("rsf_program_id") REFERENCES "p_rsf"."rsf_programs" ("rsf_program_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."!dep-rsf_program_settings" ADD CONSTRAINT "rsf_program_settings_setting_name_fkey" FOREIGN KEY ("setting_name") REFERENCES "p_rsf"."program_settings" ("setting_name") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table dashboard_exports
-- ----------------------------
ALTER TABLE "p_rsf"."dashboard_exports" ADD CONSTRAINT "exporting_cohorts_exporting_rsf_pfcbl_id_fkey" FOREIGN KEY ("exporting_rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table export_template_reports
-- ----------------------------
ALTER TABLE "p_rsf"."export_template_reports" ADD CONSTRAINT "export_template_reports_export_template_id_fkey" FOREIGN KEY ("export_template_id") REFERENCES "p_rsf"."export_templates" ("export_template_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."export_template_reports" ADD CONSTRAINT "export_template_reports_report_id_fkey" FOREIGN KEY ("report_id") REFERENCES "p_rsf"."dashboard_reports" ("report_id") ON DELETE CASCADE ON UPDATE CASCADE;

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
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_fx_indicator_exists-fkey" FOREIGN KEY ("unit_fx_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_indicator_id_category_fkey" FOREIGN KEY ("indicator_sys_category") REFERENCES "p_rsf"."indicator_sys_categories" ("indicator_sys_category") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_indicator_options_group_id_fkey" FOREIGN KEY ("indicator_options_group_id") REFERENCES "p_rsf"."indicator_options_groups" ("options_group_id") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."indicators" ADD CONSTRAINT "indicators_label_id_fkey" FOREIGN KEY ("label_id") REFERENCES "p_rsf"."label_ids" ("label_id") ON DELETE RESTRICT ON UPDATE CASCADE;
COMMENT ON CONSTRAINT "indicators_fx_indicator_exists-fkey" ON "p_rsf"."indicators" IS 'Must delete FX indicators that use this before deleting this';

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
ALTER TABLE "p_rsf"."rsf_data" ADD CONSTRAINT "rsf_data-rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
COMMENT ON CONSTRAINT "rsf_data-reporting_cohort_id_fkey" ON "p_rsf"."rsf_data" IS 'For deleted data, this fk requires cohorts are deleted FIRST and data is deleted via the deleted cohort and appropriate triggers are appropriately executed';

-- ----------------------------
-- Foreign Keys structure for table rsf_data_calculation_evaluations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_calculation_evaluations" ADD CONSTRAINT "calculation_evaluations-id-fk" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_data_calculation_evaluations" ADD CONSTRAINT "calculation_evaluations-pf_id_indicator-fk" FOREIGN KEY ("rsf_pf_id", "indicator_id") REFERENCES "p_rsf"."rsf_setup_indicators" ("rsf_pfcbl_id", "indicator_id") ON DELETE CASCADE ON UPDATE NO ACTION;
COMMENT ON CONSTRAINT "calculation_evaluations-pf_id_indicator-fk" ON "p_rsf"."rsf_data_calculation_evaluations" IS 'rsf_setup_indicator''s rsf_pfcbl_id is same as rsf_pfcbl_id (an historic misnaming)';

-- ----------------------------
-- Foreign Keys structure for table rsf_data_calculation_validations
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_calculation_validations" ADD CONSTRAINT "rsf_data_calculation_validations_data_id_fkey" FOREIGN KEY ("data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks-for_import_id-fkey" FOREIGN KEY ("for_import_id") REFERENCES "p_rsf"."reporting_imports" ("import_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks-formula_null_or_exists" FOREIGN KEY ("check_formula_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id") ON DELETE RESTRICT ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks-rsf_pfcbl_id" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_data_id_fkey" FOREIGN KEY ("data_id") REFERENCES "p_rsf"."rsf_data" ("data_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_checks" ADD CONSTRAINT "rsf_data_checks_indicator_check_id_fkey" FOREIGN KEY ("indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE RESTRICT ON UPDATE NO ACTION;
COMMENT ON CONSTRAINT "rsf_data_checks-for_import_id-fkey" ON "p_rsf"."rsf_data_checks" IS 'Should be deleted by cohort delete trigger';
COMMENT ON CONSTRAINT "rsf_data_checks_data_id_fkey" ON "p_rsf"."rsf_data_checks" IS 'RSF_DATA as non-current data points may have had checks that have actions set or notes';

-- ----------------------------
-- Foreign Keys structure for table rsf_data_checks_archive
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_check_formula_id_fkey" FOREIGN KEY ("check_formula_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_indicator_check_id_fkey" FOREIGN KEY ("indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE RESTRICT ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_data_checks_archive" ADD CONSTRAINT "rsf_data_checks_archive_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE SET NULL ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_current
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current" ADD CONSTRAINT "rsf_data_current-data_id_IN_rsf_data" FOREIGN KEY ("data_id") REFERENCES "p_rsf"."rsf_data" ("data_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_current" ADD CONSTRAINT "rsf_data_current-data_unit_data_id_IN_rsf_data_current" FOREIGN KEY ("data_unit_data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_current_fx
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_fx" ADD CONSTRAINT "rsf_data_current_fx_fx_data_id_fkey" FOREIGN KEY ("fx_data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;

-- ----------------------------
-- Foreign Keys structure for table rsf_data_current_lcu
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_lcu" ADD CONSTRAINT "rsf_data_current_lcu_for_rsf_pfcbl_id_fkey" FOREIGN KEY ("for_rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_data_current_lcu" ADD CONSTRAINT "rsf_data_current_lcu_lcu_unit_data_id_fkey" FOREIGN KEY ("lcu_unit_data_id") REFERENCES "p_rsf"."rsf_data_current" ("data_id") ON DELETE NO ACTION ON UPDATE NO ACTION DEFERRABLE INITIALLY DEFERRED;
COMMENT ON CONSTRAINT "rsf_data_current_lcu_lcu_unit_data_id_fkey" ON "p_rsf"."rsf_data_current_lcu" IS 'Enables historic updates to cascade into the future';

-- ----------------------------
-- Foreign Keys structure for table rsf_data_current_names_and_ids
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_data_current_names_and_ids" ADD CONSTRAINT "rsf_data_current_nids-cohort_id-fk" FOREIGN KEY ("data_cohort_id") REFERENCES "p_rsf"."reporting_cohorts" ("reporting_cohort_id") ON DELETE CASCADE ON UPDATE NO ACTION;

-- ----------------------------
-- Foreign Keys structure for table rsf_facilities
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_facilities" ADD CONSTRAINT "rsf_facilities_rsf_program_id_fkey" FOREIGN KEY ("rsf_program_id") REFERENCES "p_rsf"."rsf_programs" ("rsf_program_id") ON DELETE NO ACTION ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;

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
-- Foreign Keys structure for table rsf_setup_checks
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "rsf_setup_checks-cohort_id" FOREIGN KEY ("auto_subscribed_by_reporting_cohort_id") REFERENCES "p_rsf"."reporting_cohorts" ("reporting_cohort_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "rsf_setup_checks_check_formula_id_fkey" FOREIGN KEY ("check_formula_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_checks" ADD CONSTRAINT "rsf_setup_checks_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_setup_checks_config
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config-check_id_fkey" FOREIGN KEY ("indicator_check_id") REFERENCES "p_rsf"."indicator_checks" ("indicator_check_id") ON DELETE NO ACTION ON UPDATE RESTRICT;
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config-indicator_id_fkey" FOREIGN KEY ("for_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_checks_config" ADD CONSTRAINT "rsf_setup_checks_config-rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_setup_indicators
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_cohort_id" FOREIGN KEY ("auto_subscribed_by_reporting_cohort_id") REFERENCES "p_rsf"."reporting_cohorts" ("reporting_cohort_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_formula_id_fkey" FOREIGN KEY ("formula_id") REFERENCES "p_rsf"."indicator_formulas" ("formula_id") ON DELETE CASCADE ON UPDATE CASCADE DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_indicator_id_fkey" FOREIGN KEY ("indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_indicators" ADD CONSTRAINT "rsf_setup_indicators_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;

-- ----------------------------
-- Foreign Keys structure for table rsf_setup_template_headers
-- ----------------------------
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "rsf_setup_template_headers_headers_map_check_formula" FOREIGN KEY ("map_check_formula_id") REFERENCES "p_rsf"."indicator_check_formulas" ("check_formula_id") ON DELETE NO ACTION ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "rsf_setup_template_headers_headers_map_indicator_id" FOREIGN KEY ("map_indicator_id") REFERENCES "p_rsf"."indicators" ("indicator_id") ON DELETE NO ACTION ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "rsf_setup_template_headers_map_calculation_formula" FOREIGN KEY ("map_formula_id") REFERENCES "p_rsf"."indicator_formulas" ("formula_id") ON DELETE NO ACTION ON UPDATE NO ACTION;
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "rsf_setup_template_headers_rsf_pfcbl_id_fkey" FOREIGN KEY ("rsf_pfcbl_id") REFERENCES "p_rsf"."rsf_pfcbl_ids" ("rsf_pfcbl_id") ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE "p_rsf"."rsf_setup_template_headers" ADD CONSTRAINT "rsf_setup_template_headers_template_id_fkey" FOREIGN KEY ("template_id") REFERENCES "p_rsf"."reporting_templates" ("template_id") ON DELETE CASCADE ON UPDATE CASCADE;
