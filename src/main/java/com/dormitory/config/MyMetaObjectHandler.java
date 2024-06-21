package com.dormitory.config;

import cn.dev33.satoken.stp.StpUtil;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import com.dormitory.common.Constant;
import com.dormitory.entity.SysAdmin;
import com.dormitory.entity.SysStudent;
import org.apache.ibatis.reflection.MetaObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.Date;


/**
 * 配置自动填充时间
 *
 * @author XXX
 * @since 2024-05-07
 */
@Component
//@Slf4j
public class MyMetaObjectHandler implements MetaObjectHandler {

    private static final Logger log = LoggerFactory.getLogger(MyMetaObjectHandler.class);

    /**
     * 新增数据时对字段的自动填充
     *
     * @param metaObject metaObject
     */
    @Override
    public void insertFill(MetaObject metaObject) {
        log.info("start insert fill ....");
        SysAdmin admin;
        SysStudent student;
        try {
            admin = JSON.parseObject(String.valueOf(JSON.toJSON(StpUtil.getSession().get("admin"))), SysAdmin.class);
        } catch (Exception e) {
            admin = null;
        }
        try {
            student = JSON.parseObject(String.valueOf(JSON.toJSON(StpUtil.getSession().get("student"))), SysStudent.class);
        } catch (Exception e) {
            student = null;
        }
        if (admin != null) {
            // 起始版本 3.3.0(推荐使用)
            this.strictInsertFill(metaObject, "createId", Long.class, admin.getAdminId());
            this.strictInsertFill(metaObject, "createName", String.class, admin.getAdminName());
            this.strictInsertFill(metaObject, "createTime", Date.class, new Date());
            this.strictInsertFill(metaObject, "deleteFlag", Integer.class, Constant.INTEGER_ZERO);
            this.setFieldValByName("deleteFlag", Constant.DELETE_FALSE, metaObject);
        } else if (student != null){
            // 起始版本 3.3.0(推荐使用)
            this.strictInsertFill(metaObject, "createId", Long.class, student.getStudentId());
            this.strictInsertFill(metaObject, "createName", String.class, student.getStudentName());
            this.strictInsertFill(metaObject, "createTime", Date.class, new Date());
            this.strictInsertFill(metaObject, "deleteFlag", Integer.class, Constant.INTEGER_ZERO);
            this.setFieldValByName("deleteFlag", Constant.DELETE_FALSE, metaObject);
        } else {
            // 起始版本 3.3.0(推荐使用)
            this.strictInsertFill(metaObject, "createId", Long.class, 0L);
            this.strictInsertFill(metaObject, "createName", String.class, "系统");
            this.strictInsertFill(metaObject, "createTime", Date.class, new Date());
            this.strictInsertFill(metaObject, "deleteFlag", Integer.class, Constant.INTEGER_ZERO);
            this.setFieldValByName("deleteFlag", Constant.DELETE_FALSE, metaObject);
        }
        // 修改一起填充
        updateFill(metaObject);
    }

    /**
     * 修改数据时对字段的自动填充
     *
     * @param metaObject metaObject
     */
    @Override
    public void updateFill(MetaObject metaObject) {
        log.info("start update fill ....");
        SysAdmin admin;
        SysStudent student;
        try {
            admin = JSON.parseObject(String.valueOf(JSON.toJSON(StpUtil.getSession().get("admin"))), SysAdmin.class);
        } catch (Exception e) {
            admin = null;
        }
        try {
            student = JSON.parseObject(String.valueOf(JSON.toJSON(StpUtil.getSession().get("student"))), SysStudent.class);
        } catch (Exception e) {
            student = null;
        }
        if (admin != null) {
            // 起始版本 3.3.0(推荐)
            this.strictUpdateFill(metaObject, "updateId", Long.class, admin.getAdminId());
            this.strictUpdateFill(metaObject, "updateName", String.class, admin.getAdminName());
            this.strictUpdateFill(metaObject, "updateTime", Date.class, new Date());
            // 增加方法，防止更新时存在该字段不覆盖原值
            this.setFieldValByName("updateId", admin.getAdminId(), metaObject);
            this.setFieldValByName("updateName", admin.getAdminId(), metaObject);
            this.setFieldValByName("updateTime", new Date(), metaObject);
        } else if (student != null){
            // 起始版本 3.3.0(推荐)
            this.strictUpdateFill(metaObject, "updateId", Long.class, student.getStudentId());
            this.strictUpdateFill(metaObject, "updateName", String.class, student.getStudentName());
            this.strictUpdateFill(metaObject, "updateTime", Date.class, new Date());
            // 增加方法，防止更新时存在该字段不覆盖原值
            this.setFieldValByName("updateId",  student.getStudentId(), metaObject);
            this.setFieldValByName("updateName",  student.getStudentName(), metaObject);
            this.setFieldValByName("updateTime", new Date(), metaObject);
        } else {
            // 起始版本 3.3.0(推荐)
            this.strictUpdateFill(metaObject, "updateId", Long.class, 0L);
            this.strictUpdateFill(metaObject, "updateName", String.class, "系统");
            this.strictUpdateFill(metaObject, "updateTime", Date.class, new Date());
            // 增加方法，防止更新时存在该字段不覆盖原值
            this.setFieldValByName("updateId", 0L, metaObject);
            this.setFieldValByName("updateName", "系统", metaObject);
            this.setFieldValByName("updateTime", new Date(), metaObject);
        }
    }

}
