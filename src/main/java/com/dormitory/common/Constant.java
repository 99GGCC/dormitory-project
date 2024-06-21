package com.dormitory.common;

/**
 * 常量定义
 *
 * @author XXX
 */
public class Constant {

    private Constant() {
    }

    /**
     * 返回操作成功
     */
    public static final String SUCCESS = "操作成功!";

    /**
     * 返回成功的CODE
     */
    public static final int SUCCESS_CODE = 200;

    /**
     * 返回操作失败
     */
    public static final String FAIL = "操作失败!";

    /**
     * 返回失败的CODE
     */
    public static final int FAIL_CODE = 1;

    /**
     * 返回失败的CODE
     */
    public static final int NOT_LOGIN_CODE = 0;

    /**
     * 定义最大库存
     */
    public final static Integer MAX_INVENTORY = 999;

    /**
     * 定义最小库存
     */
    public final static Integer MIN_INVENTORY = 0;

    /**
     * 重置密码
     */
    public static final String RESET_PASSWORD = "123456";

    /**
     * 未删除标志位
     */
    public static final Integer DELETE_FALSE = 0;

    /**
     * INTEGER类型0
     */
    public static final Integer INTEGER_ZERO = 0;

    /**
     * INTEGER类型1
     */
    public static final Integer INTEGER_ONE = 1;

    /**
     * INTEGER类型2
     */
    public static final Integer INTEGER_TWO = 2;

    /**
     * INTEGER类型3
     */
    public static final Integer INTEGER_THREE = 3;

    /**
     * INTEGER类型4
     */
    public static final Integer INTEGER_FOUR = 4;

    /**
     * 时间格式：年-月-日
     */
    public static final String DATE_YYYY_MM_DD = "yyyy-MM-dd";

    /**
     * 时间格式：年-月-日 时:分:秒
     */
    public static final String DATE_YYYY_MM_DD_HH_MM_SS = "yyyy-MM-dd HH:mm:ss";

    /**
     * 轮播图参数CLASS
     */
    public static final Object PARAM_CAROUSE_CLASS = "PARAM_CAROUSE";

    /**
     * 状态
     */
    public static final Integer STATUS_FALSE = 0;
    public static final Integer STATUS_TRUE = 1;

}
