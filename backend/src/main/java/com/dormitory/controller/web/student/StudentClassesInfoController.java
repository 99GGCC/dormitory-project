package com.dormitory.controller.web.student;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.qry.ClassesInfoQry;
import com.dormitory.controller.vo.ClassesInfoVO;
import com.dormitory.service.ClassesInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * <p>
 * 班级信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/student/classes/info")
@Api(value = "StudentClassesInfoController", tags = {"班级信息接口"})
public class StudentClassesInfoController {
    /**
     * 班级信息Service
     */
    private final ClassesInfoService classesInfoService;

    @ApiOperation("班级信息列表查询")
    @GetMapping("/list")
    public R<List<ClassesInfoVO>> listByQry(ClassesInfoQry qry) {
        return R.success(classesInfoService.listByQry(qry));
    }

    @ApiOperation("班级信息详情")
    @GetMapping("/detail/{classesId}")
    public R<ClassesInfoVO> detail(@PathVariable @NotNull(message = "班级ID") Long classesId) {
        return R.success(classesInfoService.detailById(classesId));
    }
}

