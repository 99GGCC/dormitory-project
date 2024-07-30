package com.dormitory.service.impl;

import cn.dev33.satoken.stp.SaTokenInfo;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.Constant;
import com.dormitory.config.StpStudentUtil;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.dto.StudentLoginDTO;
import com.dormitory.controller.dto.StudentMineDTO;
import com.dormitory.controller.vo.StudentLoginVO;
import com.dormitory.controller.vo.StudentVO;
import com.dormitory.entity.SysStudent;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.SysStudentMapper;
import com.dormitory.service.SysStudentService;
import com.dormitory.utils.CopyUtils;
import com.dormitory.utils.RedisUtil;
import com.dormitory.utils.SaltUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * <p>
 * 学生信息表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class SysStudentServiceImpl extends ServiceImpl<SysStudentMapper, SysStudent> implements SysStudentService {

    /**
     * redis工具类
     */
    private final RedisUtil redisUtil;

    /**
     * 学生登录
     *
     * @param loginDTO 学生登录DTO
     * @return StudentLoginVO
     */
    @Override
    public StudentLoginVO login(StudentLoginDTO loginDTO) {
        // 判断验证码是否正确
        if (redisUtil.hasKey(Constant.SMS_CODE + loginDTO.getCode().toLowerCase())) {
            // 清除验证码
            redisUtil.del(Constant.SMS_CODE + loginDTO.getCode().toLowerCase());
            // 登录校验
            SysStudent student = new LambdaQueryChainWrapper<>(baseMapper)
                    .eq(SysStudent::getStudentNum, loginDTO.getStudentNum())
                    .one();
            // 判断学生是否为空
            if (student != null) {
                // 密码校验
                if (SaltUtils.verify(loginDTO.getStudentPass(), student.getStudentSalt(), student.getStudentPass())) {
                    // 调用登录返回值方法
                    return getLoginVO(student);
                } else {
                    // 如果密码错误，返回消息
                    throw new ServiceException("学号或密码错误!");
                }
            } else {
                throw new ServiceException("学号或密码错误!");
            }
        } else {
            throw new ServiceException("验证码错误!");
        }
    }

    /**
     * 学生个人信息
     *
     * @return StudentVO
     */
    @Override
    public StudentVO mine() {
        return baseMapper.selectByStudentId(StpStudentUtil.getLoginIdAsLong());
    }

    /**
     * 修改个人信息
     *
     * @param mineDTO 个人信息DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean editMine(StudentMineDTO mineDTO) {
        // 修改个人信息（手机号码、邮箱）
        return new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(SysStudent::getStudentId, StpStudentUtil.getLoginIdAsLong())
                .set(SysStudent::getStudentPhone, mineDTO.getStudentPhone())
                .set(SysStudent::getStudentEmail, mineDTO.getStudentEmail())
                .update();
    }

    /**
     * 修改登录密码
     *
     * @param changeDTO 修改密码DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean changePassword(ChangePasswordDTO changeDTO) {
        long studentId = StpStudentUtil.getLoginIdAsLong();
        SysStudent student = baseMapper.selectById(studentId);
        // 密码校验
        if (SaltUtils.verify(changeDTO.getOldPass(), student.getStudentSalt(), student.getStudentPass())) {
            // 加密新密码并保存数据库
            new LambdaUpdateChainWrapper<>(baseMapper)
                    .eq(SysStudent::getStudentId, studentId)
                    .set(SysStudent::getStudentPass, SaltUtils.md5Password(changeDTO.getNewPass(), student.getStudentSalt()))
                    .update();
            // 登录失效
            StpStudentUtil.logout();
            // 返回修改成功
            return true;
        } else {
            // 如果密码错误，返回消息
            throw new ServiceException("旧登录密码错误!");
        }
    }

    /**
     * 获取登录数据
     *
     * @param student 学生信息
     * @return StudentLoginVO
     */
    private StudentLoginVO getLoginVO(SysStudent student) {
        // 初始化学生登录VO
        StudentLoginVO loginVO = CopyUtils.classCopy(student, StudentLoginVO.class);
        // 登录实现
        StpStudentUtil.login(student.getStudentId());
        // 缓存学生信息
        StpStudentUtil.getSession().set("student", student);
        // 获取登录用户信息
        SaTokenInfo tokenInfo = StpStudentUtil.getTokenInfo();
        // 组装学生登录返回VO并返回
        return loginVO.setSaToken(tokenInfo.getTokenValue())
                .setTokenInfo(tokenInfo);
    }
}
