package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.controller.dto.ClassesInfoDTO;
import com.dormitory.controller.qry.ClassesInfoQry;
import com.dormitory.controller.vo.ClassesInfoVO;
import com.dormitory.entity.ClassesInfo;
import com.dormitory.entity.MajorInfo;
import com.dormitory.entity.SysStudent;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.ClassesInfoMapper;
import com.dormitory.mapper.SysStudentMapper;
import com.dormitory.service.ClassesInfoService;
import com.dormitory.utils.CopyUtils;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 * 班级信息表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class ClassesInfoServiceImpl extends ServiceImpl<ClassesInfoMapper, ClassesInfo> implements ClassesInfoService {

    /**
     * 学生信息Mapper
     */
    private final SysStudentMapper sysStudentMapper;
    /**
     * 班级信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<ClassesInfoVO>
     */
    @Override
    public IPage<ClassesInfoVO> pageByQry(ClassesInfoQry qry) {
        IPage<ClassesInfo> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getClassesName()), ClassesInfo::getClassesName, qry.getClassesName())
                .like(StringUtils.isNotEmpty(qry.getInstructorName()), ClassesInfo::getInstructorName, qry.getInstructorName())
                .like(StringUtils.isNotEmpty(qry.getInstructorPhone()), ClassesInfo::getInstructorPhone, qry.getInstructorPhone())
                .eq(ObjectUtils.isNotEmpty(qry.getMajorId()), ClassesInfo::getMajorId, qry.getMajorId())
                .eq(ObjectUtils.isNotEmpty(qry.getClassesStatus()), ClassesInfo::getClassesStatus, qry.getClassesStatus())
                .page(pages);
        return CopyUtils.covertPage(pages, ClassesInfoVO.class);
    }

    /**
     * 班级信息列表查询
     *
     * @param qry 查询Qry
     * @return List<ClassesInfoVO>
     */
    @Override
    public List<ClassesInfoVO> listByQry(ClassesInfoQry qry) {
        List<ClassesInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getClassesName()), ClassesInfo::getClassesName, qry.getClassesName())
                .list();
        return CopyUtils.classCopyList(list, ClassesInfoVO.class);
    }

    /**
     * 班级信息详情
     *
     * @param classesId 班级ID
     * @return ClassesInfoVO
     */
    @Override
    public ClassesInfoVO detailById(Long classesId) {
        return CopyUtils.classCopy(baseMapper.selectById(classesId), ClassesInfoVO.class);
    }

    /**
     * 新增班级信息
     *
     * @param dto 班级DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(ClassesInfoDTO dto) {
        // 判断班级信息是否存在
        List<ClassesInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(ClassesInfo::getClassesName, dto.getClassesName())
                .eq(ClassesInfo::getMajorId, dto.getMajorId())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("班级信息已存在,新增失败!");
        }
        // 复制班级信息实体
        ClassesInfo classesInfo = CopyUtils.classCopy(dto, ClassesInfo.class);
        // 保存班级信息，并返回结果
        return baseMapper.insert(classesInfo) > 0;
    }

    /**
     * 编辑班级信息
     *
     * @param classesId 班级ID
     * @param dto       班级DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long classesId, ClassesInfoDTO dto) {
        // 判断班级信息是否存在
        List<ClassesInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(ClassesInfo::getClassesName, dto.getClassesName())
                .ne(ClassesInfo::getClassesId, classesId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("班级信息已存在,编辑失败!");
        }
        // 复制班级信息实体
        ClassesInfo classesInfo = CopyUtils.classCopy(dto, ClassesInfo.class);
        // 设置班级ID
        classesInfo.setClassesId(classesId);
        // 编辑班级信息，并返回结果
        return baseMapper.updateById(classesInfo) > 0;
    }

    /**
     * 删除班级信息
     *
     * @param classesId 班级ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long classesId) {
        List<SysStudent> list = new LambdaQueryChainWrapper<>(sysStudentMapper)
                .eq(SysStudent::getClassesId, classesId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("该班级已被学生使用,删除失败!");
        }
        // 删除班级信息，并返回结果
        return baseMapper.deleteById(classesId) > 0;
    }
}
