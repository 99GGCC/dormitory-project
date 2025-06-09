package com.dormitory.controller.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;


/**
 * <p>
 * 考勤记录VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "考勤记录VO")
public class SignInRecordVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 记录ID
     */
    @ApiModelProperty("记录ID")
    private Long recordId;

    /**
     * 考勤ID
     */
    @ApiModelProperty("考勤ID")
    private Long signInId;

    /**
     * 学生ID
     */
    @ApiModelProperty("学生ID")
    private Long studentId;

    /**
     * 考勤状态
     */
    @ApiModelProperty("考勤状态")
    private Integer recordStatus;

    /**
     * 考勤时间
     */
    @ApiModelProperty("考勤时间")
    private Date recordTime;

    /**
     * 发布时间
     */
    @ApiModelProperty("发布时间")
    private Date issueTime;

    /**
     * 发布人
     */
    @ApiModelProperty("发布人")
    private Long adminId;

    /**
     * 考勤总人数
     */
    @ApiModelProperty("考勤总人数")
    private Integer totalStudent;

    /**
     * 实际签到人数
     */
    @ApiModelProperty("实际签到人数")
    private Integer realityStudent;

    /**
     * 考勤截止时间
     */
    @ApiModelProperty("考勤截止时间")
    private Date endTime;

    /**
     * 考勤状态
     */
    @ApiModelProperty("考勤状态")
    private Integer signInStatus;

    /**
     * 床位ID
     */
    @ApiModelProperty("床位ID")
    private Long bedId;

    /**
     * 床位名称
     */
    @ApiModelProperty("床位名称")
    private String bedName;

    /**
     * 宿舍ID
     */
    @ApiModelProperty("宿舍ID")
    private Long dormitoryId;

    /**
     * 床位状态：1、启用 0、禁用
     */
    @ApiModelProperty("床位状态：1、启用 0、禁用")
    private Integer bedStatus;

    /**
     * 是否宿舍长
     */
    @ApiModelProperty("是否宿舍长")
    private Integer isHead;

    /**
     * 宿舍名称
     */
    @ApiModelProperty("宿舍名称")
    private String dormitoryName;

    /**
     * 楼层
     */
    @ApiModelProperty("楼层")
    private Integer buildingFloor;

    /**
     * 宿舍状态：1、启用 0、禁用
     */
    @ApiModelProperty("宿舍状态：1、启用 0、禁用")
    private Integer dormitoryStatus;

    /**
     * 使用状态：1、已使用 0、未使用
     */
    @ApiModelProperty("使用状态：1、已使用 0、未使用")
    private Integer useStatus;

    /**
     * 楼栋ID
     */
    @ApiModelProperty("楼栋ID")
    private Long buildingId;

    /**
     * 楼栋名称
     */
    @ApiModelProperty("楼栋名称")
    private String buildingName;

    /**
     * 楼栋简称
     */
    @ApiModelProperty("楼栋简称")
    private String shortName;

    /**
     * 楼栋楼层
     */
    @ApiModelProperty("楼栋楼层")
    private Integer buildingFloor1;

    /**
     * 楼栋类型1、男生宿舍 0、女生宿舍
     */
    @ApiModelProperty("楼栋类型1、男生宿舍 0、女生宿舍")
    private Integer buildingType;

    /**
     * 楼层房间数量
     */
    @ApiModelProperty("楼层房间数量")
    private Integer roomNum;

    /**
     * 楼层管理员
     */
    @ApiModelProperty("楼层管理员")
    private String buildingAdmin;

    /**
     * 联系电话
     */
    @ApiModelProperty("联系电话")
    private String buildingPhone;

    /**
     * 学生姓名
     */
    @ApiModelProperty("学生姓名")
    private String studentName;

    /**
     * 学生学号
     */
    @ApiModelProperty("学生学号")
    private String studentNum;

    /**
     * 手机号码
     */
    @ApiModelProperty("手机号码")
    private String studentPhone;

    /**
     * 学生邮箱
     */
    @ApiModelProperty("学生邮箱")
    private String studentEmail;

    /**
     * 学生性别：0、女 1、男
     */
    @ApiModelProperty("学生性别：0、女 1、男")
    private Integer studentSex;

    /**
     * 班级ID
     */
    @ApiModelProperty("班级ID")
    private Long classesId;

    /**
     * 班级名称
     */
    @ApiModelProperty("班级名称")
    private String classesName;

    /**
     * 辅导员姓名
     */
    @ApiModelProperty("辅导员姓名")
    private String instructorName;

    /**
     * 联系电话
     */
    @ApiModelProperty("联系电话")
    private String instructorPhone;

    /**
     * 班级状态
     */
    @ApiModelProperty("班级状态")
    private Integer classesStatus;

    /**
     * 专业ID
     */
    @ApiModelProperty("专业ID")
    private Long majorId;

    /**
     * 专业名称
     */
    @ApiModelProperty("专业名称")
    private String majorName;

    /**
     * 学院ID
     */
    @ApiModelProperty("学院ID")
    private Long collegeId;

    /**
     * 学院名称
     */
    @ApiModelProperty("学院名称")
    private String collegeName;
}
