SELECT
  v_organizationuser.OrganizationUserId user_id,
  project_course_config.Name project_name,
  project_course_user.Progress project_progress
FROM
  iquizoo_content_db.project_course_user
  INNER JOIN iquizoo_content_db.project_course_config ON project_course_config.Id = project_course_user.ProjectCourseConfigId
  INNER JOIN iquizoo_content_db.course ON course.Id = project_course_config.CourseId
  INNER JOIN iquizoo_content_db.v_organizationuser ON v_organizationuser.OrganizationUserId = project_course_user.OrganizationUserId
  INNER JOIN iquizoo_user_db.base_organization ON base_organization.Id = v_organizationuser.OrganizationId
{ where_clause };
