package com.dormitory.service.impl;

import cn.dev33.satoken.stp.SaTokenInfo;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.*;
import com.dormitory.config.StpStudentUtil;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.dto.StudentDTO;
import com.dormitory.controller.dto.StudentLoginDTO;
import com.dormitory.controller.dto.StudentMineDTO;
import com.dormitory.controller.qry.StudentQry;
import com.dormitory.controller.vo.StudentLoginVO;
import com.dormitory.controller.vo.StudentVO;
import com.dormitory.entity.*;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.*;
import com.dormitory.service.SysStudentService;
import com.dormitory.utils.CopyUtils;
import com.dormitory.utils.IdUtils;
import com.dormitory.utils.RedisUtil;
import com.dormitory.utils.SaltUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;

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
     * 宿舍信息Mapper
     */
    private final DormitoryInfoMapper dormitoryInfoMapper;

    /**
     * 床位信息Mapper
     */
    private final BedInfoMapper bedInfoMapper;

    /**
     * 动迁记录Mapper
     */
    private final RelocationRecordMapper relocationRecordMapper;

    /**
     * 调换申请Mapper
     */
    private final ChangeApplyMapper changeApplyMapper;

    /**
     * 学生信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<StudentVO>
     */
    @Override
    public IPage<StudentVO> pageByQry(StudentQry qry) {
        return baseMapper.pageByQry(qry, new Page<>(qry.getPage(), qry.getLimit()));
    }

    /**
     * 学生信息详情
     *
     * @param studentId 学生ID
     * @return StudentVO
     */
    @Override
    public StudentVO detailById(Long studentId) {
        return baseMapper.detailById(studentId);
    }

    /**
     * 新增学生信息
     *
     * @param dto 学生DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(StudentDTO dto) {
        // 通过学号判断学生是否存在
        List<SysStudent> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(SysStudent::getStudentNum, dto.getStudentNum())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("学生已存在，请勿重复添加!");
        }
        // 生成学生类
        SysStudent student = CopyUtils.classCopy(dto, SysStudent.class);
        // 预生成学生ID
        student.setStudentId(IdUtils.getLongId());
        // 设置加密盐
        student.setStudentSalt(SaltUtils.createSalt());
        // 设置初始密码为学号
        student.setStudentPass(SaltUtils.md5Password(dto.getStudentNum(), student.getStudentSalt()));
        // 设置学生状态为正常
        student.setStudentStatus(StudentStatusEnum.NORMAL.getCode());
        // 学生宿舍和学生床位设置
        if (!ObjectUtils.isEmpty(student.getDormitoryId()) && !ObjectUtils.isEmpty(student.getBedId())) {
            // 设置首次迁入
            setDormitoryInfoIn(student, RelocationTypeEnum.FIRST_IN);
        }
        // 保存信息并返回
        return baseMapper.insert(student) > 0;
    }

    /**
     * 编辑学生信息
     *
     * @param studentId 学生ID
     * @param dto       学生DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long studentId, StudentDTO dto) {
        // 通过学号判断学生是否存在
        List<SysStudent> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(SysStudent::getStudentNum, dto.getStudentNum())
                .ne(SysStudent::getStudentId, studentId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("学生学号已存在，修改失败!");
        }
        // 生成学生类
        SysStudent student = CopyUtils.classCopy(dto, SysStudent.class);
        // 设置学生ID
        student.setStudentId(studentId);
        // 判断床位是否变化
        SysStudent old = baseMapper.selectById(studentId);
        if (!ObjectUtils.isEmpty(student.getDormitoryId()) && !ObjectUtils.isEmpty(student.getBedId())) {
            if (ObjectUtils.isEmpty(old.getDormitoryId()) && ObjectUtils.isEmpty(old.getBedId())) {
                // 设置学生床位
                setDormitoryInfoIn(student, RelocationTypeEnum.OTHER_IN);
            } else if (!old.getBedId().equals(student.getBedId()) || !old.getDormitoryId().equals(student.getDormitoryId())) {
                // 处理学生宿舍信息
                handleStudentDormitoryInfo(old, student);
            } else {
                throw new ServiceException("学生宿舍和床位异常，请联系管理员!");
            }
        }
        // 编辑信息并返回
        return baseMapper.updateById(student) > 0;
    }

    /**
     * 删除学生信息
     *
     * @param studentId 学生ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long studentId) {
        // 查询学生信息
        SysStudent student = baseMapper.selectById(studentId);
        // 设置学生宿舍迁出信息
        setDormitoryInfoOut(student, RelocationTypeEnum.OTHER_OUT);
        // 删除学生信息
        return baseMapper.deleteById(studentId) > 0;
    }

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
     * 设置学生状态
     *
     * @param studentId 学生ID
     * @param status    学生状态
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean status(Long studentId, Integer status) {
        // 查询学生信息
        SysStudent student = baseMapper.selectById(studentId);
        LambdaUpdateWrapper<SysStudent> lqw = new LambdaUpdateWrapper<>();
        // 判断学生初始状态
        if (StudentStatusEnum.NORMAL.getCode().equals(student.getStudentStatus())) {
            // 初始状态为正常，判断设置学生状态
            if (StudentStatusEnum.GRADUATE.getCode().equals(status)) {
                // 设置学生状态为毕业
                if (!ObjectUtils.isEmpty(student.getDormitoryId()) && !ObjectUtils.isEmpty(student.getBedId())) {
                    setDormitoryInfoOut(student, RelocationTypeEnum.GRADUATE_OUT);
                }
            } else if (StudentStatusEnum.STOP.getCode().equals(status)) {
                // 设置学生状态为休学
                if (!ObjectUtils.isEmpty(student.getDormitoryId()) && !ObjectUtils.isEmpty(student.getBedId())) {
                    setDormitoryInfoOut(student, RelocationTypeEnum.OTHER_OUT);
                }
            } else {
                throw new ServiceException("无法设置学生为当前状态!");
            }
            lqw.set(SysStudent::getBedId, null).set(SysStudent::getDormitoryId, null);
        } else if (StudentStatusEnum.STOP.getCode().equals(student.getStudentStatus())) {
            // 初始状态为休学，判断设置学生状态
            if (StudentStatusEnum.NORMAL.getCode().equals(status)) {
                // 设置学生状态为正常,设置其他迁入
                if (!ObjectUtils.isEmpty(student.getDormitoryId()) && !ObjectUtils.isEmpty(student.getBedId())) {
                    setDormitoryInfoIn(student, RelocationTypeEnum.OTHER_IN);
                }
            } else {
                throw new ServiceException("无法设置学生为当前状态!");
            }
        } else {
            throw new ServiceException("无法设置学生为当前状态!");
        }
        // 更新学生状态
        lqw.set(SysStudent::getStudentStatus, status)
                .eq(SysStudent::getStudentId, studentId);
        return baseMapper.update(null, lqw) > 0;
    }

    /**
     * 设置宿舍入
     *
     * @param student        学生信息
     * @param relocationType 动迁状态
     */
    private void setDormitoryInfoIn(SysStudent student, RelocationTypeEnum relocationType) {
        // 占用新床位和新宿舍
        DormitoryInfo dormitoryInfo = dormitoryInfoMapper.selectById(student.getDormitoryId());
        // 判断宿舍是否使用
        if (UseStatusEnum.USE.getCode().equals(dormitoryInfo.getUseStatus())) {
            // 宿舍已使用，默认已存在宿舍长,只更新床位使用
            new LambdaUpdateChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getBedId, student.getBedId())
                    .set(BedInfo::getUseStudent, student.getStudentId())
                    .set(BedInfo::getIsHead, BooleanEnum.FALSE.getCode())
                    .update();
        } else {
            // 宿舍未使用，当前学生自动升级为宿舍长
            new LambdaUpdateChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getBedId, student.getBedId())
                    .set(BedInfo::getUseStudent, student.getStudentId())
                    .set(BedInfo::getIsHead, BooleanEnum.TRUE.getCode())
                    .update();
            // 更新宿舍状态为已使用
            new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                    .eq(DormitoryInfo::getDormitoryId, dormitoryInfo.getDormitoryId())
                    .set(DormitoryInfo::getUseStatus, UseStatusEnum.USE.getCode())
                    .update();
        }
        // 记录动迁记录
        RelocationRecord inRelocationRecord = new RelocationRecord()
                .setStudentId(student.getStudentId())
                .setRelocationTime(new Date())
                .setDormitoryId(student.getDormitoryId())
                .setBedId(student.getBedId())
                .setRelocationType(relocationType.getCode());
        // 保存迁入
        relocationRecordMapper.insert(inRelocationRecord);
    }

    /**
     * 设置宿舍迁出
     *
     * @param student        学生信息
     * @param relocationType 动迁状态
     */
    private void setDormitoryInfoOut(SysStudent student, RelocationTypeEnum relocationType) {
        // 释放原床位和原宿舍
        BedInfo bedInfo = bedInfoMapper.selectById(student.getBedId());
        // 判断是否是宿舍长
        if (BooleanEnum.TRUE.getCode().equals(bedInfo.getIsHead())) {
            // 是宿舍长，排除当前学生查询其他学生
            List<BedInfo> list = new LambdaQueryChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getDormitoryId, student.getDormitoryId())
                    .ne(BedInfo::getBedId, bedInfo.getBedId())
                    .list();
            // 如果宿舍里还有其他学生
            if (!CollectionUtils.isEmpty(list)) {
                // 选择第一个学生自动升级为宿舍长
                new LambdaUpdateChainWrapper<>(bedInfoMapper)
                        .eq(BedInfo::getBedId, list.get(0).getBedId())
                        .set(BedInfo::getIsHead, BooleanEnum.TRUE.getCode())
                        .update();
            } else {
                // 当前宿舍不存在学生，释放宿舍
                new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                        .eq(DormitoryInfo::getDormitoryId, student.getDormitoryId())
                        .set(DormitoryInfo::getUseStatus, UseStatusEnum.NOT_USE.getCode())
                        .update();
            }
        } else {
            // 排除当前学生查询其他学生
            List<BedInfo> list = new LambdaQueryChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getDormitoryId, bedInfo.getDormitoryId())
                    .ne(BedInfo::getBedId, bedInfo.getBedId())
                    .isNotNull(BedInfo::getUseStudent)
                    .list();
            // 如果宿舍里没有学生
            if (CollectionUtils.isEmpty(list)) {
                // 当前宿舍不存在学生，释放宿舍
                new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                        .eq(DormitoryInfo::getDormitoryId, bedInfo.getDormitoryId())
                        .set(DormitoryInfo::getUseStatus, UseStatusEnum.NOT_USE.getCode())
                        .update();
            }
        }
        // 取消当前床位占用
        new LambdaUpdateChainWrapper<>(bedInfoMapper)
                .eq(BedInfo::getBedId, student.getBedId())
                .set(BedInfo::getUseStudent, null)
                .update();
        // 记录动迁记录
        RelocationRecord outRelocationRecord = new RelocationRecord()
                .setStudentId(student.getStudentId())
                .setRelocationTime(new Date())
                .setDormitoryId(student.getDormitoryId())
                .setBedId(student.getBedId())
                .setRelocationType(relocationType.getCode());
        // 保存迁出
        relocationRecordMapper.insert(outRelocationRecord);
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

    /**
     * 处理学生宿舍信息
     *
     * @param old     旧学生信息
     * @param student 新学生信息
     */
    private void handleStudentDormitoryInfo(SysStudent old, SysStudent student) {
        // 学生申请中的调换申请失效
        new LambdaUpdateChainWrapper<>(changeApplyMapper)
                .eq(ChangeApply::getStudentId, student)
                .eq(ChangeApply::getApplyStatus, ApplyStatusEnum.APPLY.getCode())
                .set(ChangeApply::getApplyStatus, ApplyStatusEnum.CANCEL.getCode())
                .set(ChangeApply::getApplyResult, "管理员已单独处理")
                .update();
        // 释放原床位和原宿舍
        BedInfo bedInfo = bedInfoMapper.selectById(old.getBedId());
        // 判断是否是宿舍长
        if (BooleanEnum.TRUE.getCode().equals(bedInfo.getIsHead())) {
            // 是宿舍长，排除当前学生查询其他学生
            List<BedInfo> list = new LambdaQueryChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getDormitoryId, old.getDormitoryId())
                    .ne(BedInfo::getBedId, bedInfo.getBedId())
                    .list();
            // 如果宿舍里还有其他学生
            if (!CollectionUtils.isEmpty(list)) {
                // 选择第一个学生自动升级为宿舍长
                new LambdaUpdateChainWrapper<>(bedInfoMapper)
                        .eq(BedInfo::getBedId, list.get(0).getBedId())
                        .set(BedInfo::getIsHead, BooleanEnum.TRUE.getCode())
                        .update();
            } else {
                // 当前宿舍不存在学生，释放宿舍
                new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                        .eq(DormitoryInfo::getDormitoryId, old.getDormitoryId())
                        .set(DormitoryInfo::getUseStatus, UseStatusEnum.NOT_USE.getCode())
                        .update();
            }
        } else {
            // 排除当前学生查询其他学生
            List<BedInfo> list = new LambdaQueryChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getDormitoryId, bedInfo.getDormitoryId())
                    .ne(BedInfo::getBedId, bedInfo.getBedId())
                    .isNotNull(BedInfo::getUseStudent)
                    .list();
            // 如果宿舍里没有学生
            if (CollectionUtils.isEmpty(list)) {
                // 当前宿舍不存在学生，释放宿舍
                new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                        .eq(DormitoryInfo::getDormitoryId, bedInfo.getDormitoryId())
                        .set(DormitoryInfo::getUseStatus, UseStatusEnum.NOT_USE.getCode())
                        .update();
            }
        }
        // 取消当前床位占用
        new LambdaUpdateChainWrapper<>(bedInfoMapper)
                .eq(BedInfo::getBedId, old.getBedId())
                .set(BedInfo::getUseStudent, null)
                .update();
        // 占用新床位和新宿舍
        DormitoryInfo dormitoryInfo = dormitoryInfoMapper.selectById(student.getDormitoryId());
        // 判断宿舍是否使用
        if (UseStatusEnum.USE.getCode().equals(dormitoryInfo.getUseStatus())) {
            // 宿舍已使用，默认已存在宿舍长,只更新床位使用
            new LambdaUpdateChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getBedId, student.getBedId())
                    .set(BedInfo::getUseStudent, student.getStudentId())
                    .set(BedInfo::getIsHead, BooleanEnum.FALSE.getCode())
                    .update();
        } else {
            // 宿舍未使用，当前学生自动升级为宿舍长
            new LambdaUpdateChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getBedId, student.getBedId())
                    .set(BedInfo::getUseStudent, student.getStudentId())
                    .set(BedInfo::getIsHead, BooleanEnum.TRUE.getCode())
                    .update();
            // 更新宿舍状态为已使用
            new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                    .eq(DormitoryInfo::getDormitoryId, dormitoryInfo.getDormitoryId())
                    .set(DormitoryInfo::getUseStatus, UseStatusEnum.USE.getCode())
                    .update();
        }
        // 更新学生信息中的床位信息
        new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(SysStudent::getStudentId, student.getStudentId())
                .set(SysStudent::getDormitoryId, student.getDormitoryId())
                .set(SysStudent::getBedId, student.getBedId())
                .update();
        // 记录动迁记录(调换迁出)
        RelocationRecord outRelocationRecord = new RelocationRecord()
                .setStudentId(old.getStudentId())
                .setRelocationTime(new Date())
                .setDormitoryId(old.getDormitoryId())
                .setBedId(old.getBedId())
                .setRelocationType(RelocationTypeEnum.CHANGE_OUT.getCode());
        // 保存调换迁出
        relocationRecordMapper.insert(outRelocationRecord);
        // 记录动迁记录(调换迁入)
        RelocationRecord inRelocationRecord = new RelocationRecord()
                .setStudentId(student.getStudentId())
                .setRelocationTime(new Date())
                .setDormitoryId(student.getDormitoryId())
                .setBedId(student.getBedId())
                .setRelocationType(RelocationTypeEnum.CHANGE_IN.getCode());
        // 保存调换迁入
        relocationRecordMapper.insert(inRelocationRecord);
    }
}
